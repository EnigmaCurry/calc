(ns calc.eval
  (:require [calc.units :as u]
            [calc.math :as m]
            [calc.dice :as dice]
            [clojure.string :as str]))

;; ============================================================================
;; Helpers
;; ============================================================================

(defn format-unit-label
  "Format a unit keyword or exponent map as a short canonical string."
  [unit]
  (if (keyword? unit)
    (get u/unit-short-names unit (name unit))
    (let [pos (into {} (filter (fn [[_ v]] (pos? v))) unit)
          neg (into {} (filter (fn [[_ v]] (neg? v))) unit)]
      (str (str/join "\u00b7" (for [[k v] pos]
                                (let [label (get u/unit-short-names k (name k))]
                                  (if (= v 1) label (str label "^" v)))))
           (when (seq neg)
             (str "/" (str/join "\u00b7" (for [[k v] neg]
                                          (let [label (get u/unit-short-names k (name k))]
                                            (if (= v -1) label (str label "^" (- v))))))))))))

;; ============================================================================
;; Scalar conversion
;; ============================================================================

(defn convert-scalar [value from-unit to-unit]
  (if-not (u/compatible? from-unit to-unit)
    (u/incompatible-error from-unit to-unit)
    (let [{from-scale :scale} (u/unit-spec from-unit)
          {to-scale :scale}   (u/unit-spec to-unit)
          value (u/->bigdec value)]
      (u/normalize-number
       (m/ddiv (m/d* value from-scale) to-scale)))))

;; ============================================================================
;; Temperature conversion (affine transforms)
;; ============================================================================

(def ^:private temp-offset (u/->bigdec 273.15))
(def ^:private temp-32 (u/->bigdec 32))
(def ^:private temp-5 (u/->bigdec 5))
(def ^:private temp-9 (u/->bigdec 9))

(defn c->k [c]
  (m/d+ c temp-offset))

(defn k->c [k]
  (m/d- k temp-offset))

(defn f->c [f]
  (m/d* (m/d- f temp-32) (m/ddiv temp-5 temp-9)))

(defn c->f [c]
  (m/d+ (m/d* c (m/ddiv temp-9 temp-5)) temp-32))

(defn temperature->kelvin [value unit]
  (case unit
    :K value
    :degC (c->k value)
    :degF (c->k (f->c value))))

(defn kelvin->temperature [value unit]
  (case unit
    :K value
    :degC (k->c value)
    :degF (c->f (k->c value))))

(defn convert-temperature [value from-unit to-unit]
  (if-not (and (u/temperature-units from-unit)
               (u/temperature-units to-unit))
    (let [temp-dim {:temperature 1}
          from-dim (if (u/temperature-units from-unit) temp-dim (:dim (u/unit-spec from-unit)))
          to-dim   (if (u/temperature-units to-unit)   temp-dim (:dim (u/unit-spec to-unit)))]
      {:error :incompatible-dimensions :from from-dim :to to-dim})
    (u/normalize-number
     (-> value
         (temperature->kelvin from-unit)
         (kelvin->temperature to-unit)))))

(defn temperature-request? [from-unit to-unit]
  (or (u/temperature-units from-unit)
      (u/temperature-units to-unit)))

;; ============================================================================
;; Composite conversion
;; ============================================================================

(defn convert-one [{:keys [value unit]} to-unit]
  (if (temperature-request? unit to-unit)
    (convert-temperature value unit to-unit)
    (convert-scalar value unit to-unit)))

(defn error? [x]
  (and (map? x) (contains? x :error)))

(defn convert-mixed [terms to-unit]
  (loop [remaining terms
         total 0]
    (if (empty? remaining)
      (u/normalize-number total)
      (let [converted (convert-one (first remaining) to-unit)]
        (if (error? converted)
          converted
          (recur (rest remaining) (m/d+ total converted)))))))

;; ============================================================================
;; Quantity arithmetic evaluation
;; ============================================================================

(defn- coerce-to-decimal [x]
  (m/->dec x))

(defn- evaluate-qty-expr
  "Evaluate quantity arithmetic: multiply/divide/add/subtract quantities with units,
   then convert the result to `to-unit`."
  [{:keys [terms ops]} to-unit]
  (let [first-spec (u/unit-spec (:unit (first terms)))
        result (reduce
                (fn [{:keys [value dim]} [op term]]
                  (let [spec     (u/unit-spec (:unit term))
                        term-val (m/d* (coerce-to-decimal (:value term)) (:scale spec))]
                    (case op
                      :* {:value (m/d* value term-val)
                          :dim   (u/merge-dims dim (:dim spec))}
                      :/ {:value (m/ddiv value term-val)
                          :dim   (u/merge-dims dim (u/scale-dim (:dim spec) -1))}
                      :+ (if (= dim (:dim spec))
                           {:value (m/d+ value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)}))
                      :- (if (= dim (:dim spec))
                           {:value (m/d- value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)})))))
                {:value (m/d* (coerce-to-decimal (:value (first terms))) (:scale first-spec))
                 :dim   (:dim first-spec)}
                (map vector ops (rest terms)))]
    (if (:error result)
      result
      (let [to-spec (u/unit-spec to-unit)]
        (if (= (:dim result) (:dim to-spec))
          (u/normalize-number (m/ddiv (:value result) (:scale to-spec)))
          {:error :incompatible-dimensions
           :from  (:dim result)
           :to    (:dim to-spec)})))))

;; ---------------------------------------------------------------------------
;; Auto-scaling: pick the best unit for a given dimension and SI value
;; ---------------------------------------------------------------------------

(defn- pick-best-unit
  "From a sorted candidate list, pick the largest unit where |value/scale| >= 1."
  [candidates abs-val]
  (or (->> candidates
           reverse
           (filter (fn [[_ s]]
                     (m/d>= (m/ddiv abs-val s) 1.0)))
           first)
      (first candidates)))

(defn- try-compound-unit
  "Try to decompose `dim` into num-dim / denom-dim where both are in
   auto-scale-units. Returns {:value ... :unit-label \"W/day\"} or nil."
  [dim si-value]
  (let [known-dims (keys u/auto-scale-units)
        candidates
        (for [num-dim   known-dims
              :let [remainder (u/normalize-map
                               (merge-with - dim num-dim))]
              :when (seq remainder)
              :when (every? neg? (vals remainder))
              :let [denom-dim (u/normalize-map
                               (into {} (map (fn [[k v]] [k (- v)]) remainder)))]
              :when (get u/auto-scale-units denom-dim)
              :let [num-cands   (get u/auto-scale-units num-dim)
                    denom-cands (get u/auto-scale-units denom-dim)]
              [denom-key denom-scale] denom-cands
              :let [adjusted (m/d* si-value denom-scale)
                    abs-adj  (m/dabs adjusted)
                    [num-key num-scale] (pick-best-unit num-cands abs-adj)
                    converted (u/normalize-number (m/ddiv adjusted num-scale))
                    abs-conv (m/dec->double (m/dabs converted))]]
          {:value     converted
           :abs-conv  abs-conv
           :unit-label (str (get u/unit-short-names num-key (name num-key))
                            "/"
                            (get u/unit-short-names denom-key (name denom-key)))})]
    (when (seq candidates)
      (let [score (fn [{:keys [abs-conv]}]
                    (let [range-penalty
                          (cond
                            (and (>= abs-conv 1.0) (< abs-conv 1000.0)) 0.0
                            (< abs-conv 1.0) (m/dlog10 (/ 1.0 abs-conv))
                            :else (m/dlog10 (/ abs-conv 999.0)))
                          label-penalty (* 0.001 (count (str abs-conv)))]
                      (+ range-penalty label-penalty)))
            best (apply min-key score candidates)]
        (dissoc best :abs-conv)))))

(defn- auto-select-unit
  "Given a dimension map and a value in SI base units, pick the best unit
   and return {:value converted-value :unit-label \"days\"}."
  [dim si-value]
  (if-let [candidates (get u/auto-scale-units dim)]
    (let [abs-val (m/dabs si-value)
          [unit-key scale] (pick-best-unit candidates abs-val)
          converted (u/normalize-number (m/ddiv si-value scale))]
      {:value converted :unit-label (get u/unit-display-names unit-key (name unit-key))})
    (or (try-compound-unit dim si-value)
        {:value (u/normalize-number si-value) :unit-label nil})))

(defn- expr-unit-label
  "Build a human-readable unit label from expression terms and ops.
   E.g., [{:unit :V} {:unit :W}] [:*] → \"volts·watts\""
  [terms ops]
  (let [op-sym {:* "·" :/ "/"}
        first-label (get u/unit-display-names (:unit (first terms))
                         (name (:unit (first terms))))]
    (reduce (fn [s [op term]]
              (let [label (get u/unit-display-names (:unit term)
                               (name (:unit term)))]
                (str s (get op-sym op "?") label)))
            first-label
            (map vector ops (rest terms)))))

(defn- evaluate-qty-expr-auto
  "Evaluate a quantity expression and auto-select the best output unit."
  [{:keys [terms ops]}]
  (let [first-spec (u/unit-spec (:unit (first terms)))
        result (reduce
                (fn [{:keys [value dim]} [op term]]
                  (let [spec     (u/unit-spec (:unit term))
                        term-val (m/d* (coerce-to-decimal (:value term)) (:scale spec))]
                    (case op
                      :* {:value (m/d* value term-val)
                          :dim   (u/merge-dims dim (:dim spec))}
                      :/ {:value (m/ddiv value term-val)
                          :dim   (u/merge-dims dim (u/scale-dim (:dim spec) -1))}
                      :+ (if (= dim (:dim spec))
                           {:value (m/d+ value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)}))
                      :- (if (= dim (:dim spec))
                           {:value (m/d- value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)})))))
                {:value (m/d* (coerce-to-decimal (:value (first terms))) (:scale first-spec))
                 :dim   (:dim first-spec)}
                (map vector ops (rest terms)))]
    (if (:error result)
      result
      (let [auto (auto-select-unit (:dim result) (:value result))]
        (if (:unit-label auto)
          auto
          (assoc auto :unit-label (expr-unit-label terms ops)))))))

;; ============================================================================
;; Request dispatch
;; ============================================================================

(defn- wrap-result
  "Wrap an internal result into a uniform envelope.
   Success: {:ok? true :value N} or {:ok? true :value N :unit-label \"days\"}
            or {:ok? true :mixed [{:value N :unit-label \"ft\"} ...]}
   Error:   {:ok? false :error :kind ...}"
  [result]
  (cond
    (error? result)
    (assoc result :ok? false)

    ;; Dice roll result
    (and (map? result) (contains? result :roll))
    (assoc result :ok? true)

    ;; Mixed output result
    (and (map? result) (contains? result :mixed))
    (assoc result :ok? true)

    ;; Auto-scaled result from evaluate-qty-expr-auto / auto-select-unit
    (and (map? result) (contains? result :value))
    (assoc result :ok? true)

    ;; Bare number from convert-one / convert-mixed / evaluate-qty-expr
    :else
    {:ok? true :value result}))

(defn- evaluate-percentage [{:keys [type value total percent] :as _request}]
  (case type
    :what-percent
    (let [result (u/normalize-number
                  (m/ddiv (m/d* (m/->dec value) (m/->dec 100)) (m/->dec total)))]
      {:value result :unit-label "%"})

    :percent-of
    (let [result (u/normalize-number
                  (m/ddiv (m/d* (m/->dec percent) (m/->dec value)) (m/->dec 100)))]
      {:value result})))

(defn- evaluate-root
  "Compute the nth root of a value. Returns exact integer for perfect roots,
   otherwise BigDecimal (JVM) or Decimal (ClojureScript)."
  [{:keys [degree value]}]
  #?(:clj
     (let [n (long degree)
           v (u/->bigdec value)
           neg? (neg? (double v))
           abs-v (if neg? (.negate v) v)
           approx (Math/pow (double abs-v) (/ 1.0 n))
           candidate (Math/round approx)]
       (if (and (pos? candidate)
                (= (reduce *' (repeat n (bigint candidate)))
                   (bigint abs-v)))
         (let [result (bigint candidate)]
           {:value (u/normalize-number (if (and neg? (odd? n)) (- result) result))})
         (let [result (Math/pow (double v) (/ 1.0 n))]
           {:value (u/normalize-number (bigdec result))})))
     :cljs
     (let [result (m/dpow value (m/ddiv 1.0 degree))
           rounded (m/dround result)]
       (if (m/d== result rounded)
         {:value (m/normalize rounded)}
         {:value (m/normalize result)}))))

(defn- deg->rad [x]
  (* (m/dec->double x) (/ m/PI 180.0)))

(defn- rad->deg [x]
  (* (m/dec->double x) (/ 180.0 m/PI)))

(def ^:private forward-trig-fns
  {:sin m/dsin :cos m/dcos :tan m/dtan})

(def ^:private inverse-trig-fns
  {:asin m/dasin :acos m/dacos :atan m/datan})

(defn- evaluate-trig
  "Evaluate a trig function. For forward trig (sin/cos/tan), input is in the
   given angle-mode and output is a dimensionless number. For inverse trig
   (asin/acos/atan), input is dimensionless and output is in the given angle-mode."
  [{:keys [fn value angle-mode]}]
  (let [v (m/dec->double value)]
    (if-let [f (get forward-trig-fns fn)]
      (let [rad (if (= angle-mode :rad) v (deg->rad v))
            result (f rad)
            result (if (< (Math/abs (double result)) 1e-14) 0.0 (double result))]
        {:value (u/normalize-number #?(:clj (bigdec result) :cljs result))})
      (let [f (get inverse-trig-fns fn)
            rad-result (f v)
            result (if (= angle-mode :rad)
                     rad-result
                     (rad->deg rad-result))
            result (double result)
            result (if (< (Math/abs (- result (Math/round result))) 1e-10)
                     (double (Math/round result))
                     result)]
        {:value (u/normalize-number #?(:clj (bigdec result) :cljs result))
         :unit-label (if (= angle-mode :rad) "rad" "°")}))))

(defn- evaluate-modulo [{:keys [dividend divisor]}]
  (let [result (u/normalize-number (m/dmod (m/->dec dividend) (m/->dec divisor)))]
    {:value result}))

(defn- round-up-penny
  "Round a monetary value up to the nearest cent (ceiling)."
  [x]
  #?(:clj  (u/normalize-number (.setScale (u/->bigdec x) 2 java.math.RoundingMode/CEILING))
     :cljs (u/normalize-number (m/ddiv (m/dceil (m/d* x 100)) 100))))

(defn- calc-pct
  "Calculate the effective tip percentage, rounded to 1 decimal."
  [tip bill]
  (u/normalize-number
   #?(:clj  (.setScale (m/ddiv (m/d* (m/->dec tip) (m/->dec 100)) (m/->dec bill))
                        1 java.math.RoundingMode/HALF_UP)
      :cljs (m/ddiv (m/dround (m/d* (m/ddiv tip bill) 1000)) 10))))

(defn- tip-row
  "Build a tip table row from a bill and tip amount."
  [bill tip label]
  {:label label
   :tip (u/normalize-number (m/->dec tip))
   :total (u/normalize-number (m/d+ (m/->dec bill) (m/->dec tip)))
   :percent (calc-pct tip bill)})

(defn- exact-tip-row
  "Build a row for an exact percentage."
  [bill pct]
  (let [tip (round-up-penny
             (m/ddiv (m/d* (m/->dec pct) (m/->dec bill)) (m/->dec 100)))
        display-pct (calc-pct tip bill)]
    (tip-row bill tip (str display-pct "%"))))

(defn- find-round-amount
  "Find the smallest round cash amount (multiple of $20/$10/$5/$1) in [min-val, max-val].
   Tries largest denominations first. Returns the amount or nil."
  [min-val max-val]
  (first
   (for [denom [20 10 5 1]
         :let [candidate (* denom
                            (long (m/dceil (m/ddiv (m/dec->double min-val) denom))))]
         :when (<= candidate (m/dec->double max-val))]
     (u/normalize-number (m/->dec candidate)))))

(defn- round-tip-row
  "Find a round tip amount between min-pct% and max-pct% of the bill."
  [bill min-pct max-pct]
  (let [bill-d (m/dec->double bill)
        min-tip (* bill-d (/ (m/dec->double min-pct) 100.0))
        max-tip (* bill-d (/ (m/dec->double max-pct) 100.0))]
    (when-let [tip (find-round-amount min-tip max-tip)]
      (let [pct (calc-pct tip bill)]
        (tip-row bill tip (str pct "%"))))))

(defn- round-total-row
  "Find a round total amount where the tip falls between min-pct% and max-pct%."
  [bill min-pct max-pct]
  (let [bill-d (m/dec->double bill)
        min-total (+ bill-d (* bill-d (/ (m/dec->double min-pct) 100.0)))
        max-total (+ bill-d (* bill-d (/ (m/dec->double max-pct) 100.0)))]
    (when-let [total (find-round-amount min-total max-total)]
      (let [tip (u/normalize-number (m/d- (m/->dec total) (m/->dec bill)))
            pct (calc-pct tip bill)]
        (tip-row bill tip (str pct "%"))))))

(defn- dedupe-rows
  "Remove rows with duplicate totals, keeping the first occurrence."
  [rows]
  (first
   (reduce (fn [[acc seen] row]
             (if (seen (:total row))
               [acc seen]
               [(conj acc row) (conj seen (:total row))]))
           [[] #{}]
           rows)))

(defn- evaluate-tip [{:keys [percent bill round-tip exact]}]
  (if round-tip
    ;; No explicit rate: show table with 15%, 20%, round tip, round total
    (let [rows (dedupe-rows
                 (filterv some?
                   [(exact-tip-row bill 15)
                    (exact-tip-row bill 20)
                    (round-tip-row bill 20 30)
                    (round-total-row bill 20 30)]))]
      {:value (:tip (second rows)) :rows rows :bill bill})
    ;; Explicit rate
    (let [exact-row (exact-tip-row bill percent)
          rows (if exact
                 ;; Exact total specified: only show the one result
                 [exact-row]
                 ;; Normal percent: show exact + round tip + round total
                 (dedupe-rows
                   (filterv some?
                     [exact-row
                      (round-tip-row bill percent (+ (m/dec->double percent) 10))
                      (round-total-row bill percent (+ (m/dec->double percent) 10))])))]
      {:value (:tip exact-row) :rows rows :bill bill})))

;; ============================================================================
;; Base conversion
;; ============================================================================

(defn- int->base-str
  "Convert an integer to a string in the given base (2-36). Uses lowercase."
  [n base]
  #?(:clj  (.toString (BigInteger/valueOf (long n)) (int base))
     :cljs (.toString (js/Number n) base)))

(defn- format-base-result
  "Format an integer value as a string in the target base with appropriate prefix."
  [value to-base]
  (cond
    (= to-base :decimal) (str value)
    (= to-base :hex) (str "0x" (int->base-str value 16))
    (= to-base :binary) (str "0b" (int->base-str value 2))
    (= to-base :octal) (str "0o" (int->base-str value 8))
    (= to-base :sexagesimal)
    (let [total (long value)
          h (quot total 3600)
          remainder (mod total 3600)
          m (quot remainder 60)
          s (mod remainder 60)]
      #?(:clj  (format "%d:%02d:%02d" h m s)
         :cljs (str h ":" (when (< m 10) "0") m ":" (when (< s 10) "0") s)))
    (integer? to-base) (int->base-str value to-base)
    :else (str value)))

(defn- evaluate-base-convert [{:keys [value to-base]}]
  {:value (format-base-result value to-base)})

(defn- evaluate-tax [{:keys [percent price]}]
  (let [tax (round-up-penny
             (m/ddiv (m/d* (m/->dec percent) (m/->dec price)) (m/->dec 100)))
        total (u/normalize-number (m/d+ (m/->dec price) (m/->dec tax)))]
    {:value tax :tax tax :total total}))

(defn- convert-to-mixed-units
  "Convert a single value+unit to a vector of mixed output units.
   E.g., 180 cm → [{:value 5 :unit-label \"ft\"} {:value 10.866... :unit-label \"in\"}]
   The last unit gets the fractional remainder."
  [value from-unit to-units]
  (let [;; First convert to the first target unit to get the total
        first-to (first to-units)]
    (if (temperature-request? from-unit first-to)
      ;; Temperature doesn't support mixed output
      {:error :incompatible-dimensions
       :from (:dim (u/unit-spec from-unit))
       :to "mixed units"}
      (let [;; Check all target units are compatible
            from-dim (:dim (u/unit-spec from-unit))
            _ (doseq [tu to-units]
                (when (not= from-dim (:dim (u/unit-spec tu)))
                  (throw (ex-info "Incompatible mixed target units"
                                  {:error :incompatible-dimensions
                                   :from from-dim
                                   :to (:dim (u/unit-spec tu))}))))
            ;; Convert source to SI base value
            from-spec (u/unit-spec from-unit)
            si-value (m/d* (coerce-to-decimal value) (:scale from-spec))
            ;; Cascade through target units largest-to-smallest
            results (loop [remaining-si si-value
                           units to-units
                           acc []]
                      (if (= 1 (count units))
                        ;; Last unit gets the remainder
                        (let [u (first units)
                              spec (u/unit-spec u)
                              converted (u/normalize-number
                                         (m/ddiv remaining-si (:scale spec)))]
                          (conj acc {:value converted
                                     :unit-label (format-unit-label u)}))
                        (let [u (first units)
                              spec (u/unit-spec u)
                              converted (m/ddiv remaining-si (:scale spec))
                              whole #?(:clj (bigint (long (Math/floor (double converted))))
                                       :cljs (m/dfloor converted))
                              used (m/d* (coerce-to-decimal whole) (:scale spec))
                              leftover (m/d- remaining-si used)]
                          (recur leftover
                                 (rest units)
                                 (conj acc {:value (u/normalize-number whole)
                                            :unit-label (format-unit-label u)})))))]
        {:mixed results}))))

(defn convert-request [{:keys [op quantity to] :as request}]
  (wrap-result
   (cond
     (error? request)
     request

     (= op :percentage)
     (evaluate-percentage request)

     (= op :root)
     (evaluate-root request)

     (= op :modulo)
     (evaluate-modulo request)

     (= op :trig)
     (evaluate-trig request)

     (= op :tip)
     (evaluate-tip request)

     (= op :tax)
     (evaluate-tax request)

     (= op :base-convert)
     (evaluate-base-convert request)

     (= op :math-expr)
     {:value (u/normalize-number (:value request))}

     (= op :roll)
     (let [{:keys [dice]} request]
       (if (:error dice)
         dice
         {:roll (dice/roll dice)}))

     (not= op :convert)
     {:error :unsupported-operation
      :op op}

     ;; Mixed output target (e.g., "feet and inches")
     (vector? to)
     (cond
       (vector? quantity)
       ;; Mixed input → mixed output: sum inputs first
       (let [first-to (first to)
             summed (convert-mixed quantity first-to)]
         (if (error? summed)
           summed
           (convert-to-mixed-units summed first-to to)))

       (:qty-expr quantity)
       ;; Quantity expression → mixed output
       (let [first-to (first to)
             result (evaluate-qty-expr quantity first-to)]
         (if (error? result)
           result
           (convert-to-mixed-units result first-to to)))

       (map? quantity)
       (convert-to-mixed-units (:value quantity) (:unit quantity) to)

       :else
       {:error :invalid-request :request request})

     (and (:qty-expr quantity) (= to :auto))
     (evaluate-qty-expr-auto quantity)

     (:qty-expr quantity)
     (evaluate-qty-expr quantity to)

     (and (map? quantity) (= to :auto))
     (let [unit-key (:unit quantity)]
       (if (keyword? unit-key)
         {:value (u/normalize-number (:value quantity))
          :unit-label (get u/unit-display-names unit-key (name unit-key))}
         ;; Compound unit (exponent map) — convert value to SI and auto-select
         (let [spec (u/unit-spec unit-key)
               si-value (m/d* (coerce-to-decimal (:value quantity)) (:scale spec))
               auto-result (auto-select-unit (:dim spec) si-value)]
           (if (:unit-label auto-result)
             auto-result
             ;; No auto-scale match — return original value with label from input unit
             {:value (u/normalize-number (:value quantity))
              :unit-label (format-unit-label unit-key)
              :ok? true}))))

     (vector? quantity)
     (convert-mixed quantity to)

     (map? quantity)
     (convert-one quantity to)

     :else
     {:error :invalid-request
      :request request})))
