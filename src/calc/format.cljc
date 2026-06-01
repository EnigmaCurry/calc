(ns calc.format
  #?(:clj (:import [java.math BigDecimal MathContext RoundingMode]))
  (:require [clojure.string :as str]
            [calc.math :as m]
            [calc.dice :as dice]))

;; ---------------------------------------------------------------------------
;; Fraction formatting
;; ---------------------------------------------------------------------------

(defn- best-fraction
  "Find the simplest fraction approximating x using the Stern-Brocot mediant
   algorithm. Returns [numerator denominator] or nil if no good match found."
  [x max-denom tol]
  (loop [lo-n 0 lo-d 1 hi-n 1 hi-d 0 iters 0]
    (let [mid-n (+ lo-n hi-n)
          mid-d (+ lo-d hi-d)]
      (cond
        (> mid-d max-denom) nil
        (> iters 100) nil

        (< (m/dec->double (m/dabs (- (/ (double mid-n) (double mid-d)) x)))
           tol)
        [mid-n mid-d]

        (< (/ (double mid-n) (double mid-d)) x)
        (recur mid-n mid-d hi-n hi-d (inc iters))

        :else
        (recur lo-n lo-d mid-n mid-d (inc iters))))))

(defn- format-mixed
  "Format numerator/denominator as a mixed number string."
  [n d neg?]
  (cond
    (= 1 d)
    (str (if neg? (- n) n))

    :else
    (let [whole #?(:clj (quot n d) :cljs (m/dec->double (m/dquot n d)))
          rem   #?(:clj (mod n d) :cljs (m/dec->double (m/dmod n d)))
          s (cond
              (zero? rem)   (str whole)
              (zero? whole) (str n "/" d)
              :else         (str whole " " rem "/" d))]
      (if neg? (str "-" s) s))))

(defn- format-as-fraction [x]
  (let [d (m/dec->double x)]
    (if (m/dinteger? x)
      (str (long d))
      (let [neg?  (neg? d)
            abs-d (Math/abs d)
            whole (long (Math/floor abs-d))
            frac  (- abs-d whole)]
        (if (< frac 1e-9)
          (str (if neg? (- whole) whole))
          (if-let [[n denom] (best-fraction frac 10000 1e-9)]
            (let [s (if (zero? whole)
                      (str n "/" denom)
                      (str whole " " n "/" denom))]
              (if neg? (str "-" s) s))
            ;; Fallback: no clean fraction found
            (str x)))))))

;; ---------------------------------------------------------------------------
;; Number formatting
;; ---------------------------------------------------------------------------

(defn format-number
  ([x] (format-number x nil))
  ([x {:keys [round sig-figs style numeric original-expr]}]
   (if (= :fraction style)
     (format-as-fraction x)
     #?(:clj
        (cond
          round
          (let [bd (if (instance? BigDecimal x) x (BigDecimal. (double x)))]
            (.toPlainString (.setScale bd (int round) RoundingMode/HALF_UP)))

          sig-figs
          (let [bd (if (instance? BigDecimal x) x (BigDecimal. (double x)))]
            (.toPlainString (.stripTrailingZeros
                             (.round bd (MathContext. (int sig-figs) RoundingMode/HALF_UP)))))

          :else
          (cond
            (instance? BigDecimal x)
            (let [s (.toPlainString (.stripTrailingZeros ^BigDecimal x))]
              (if (> (count s) 20)
                (.toString (.stripTrailingZeros ^BigDecimal x))
                s))

            (ratio? x)
            (let [bd (BigDecimal. (double x))
                  approx (.toPlainString (.stripTrailingZeros
                                          (.round bd (MathContext. 10 RoundingMode/HALF_UP))))
                  reduced (str x)]
              (if numeric
                approx
                (if (and original-expr
                         (not= (str/trim original-expr) reduced))
                  (str (str/trim original-expr) " = " reduced " = " approx)
                  (str reduced " = " approx))))

            :else
            (let [s (str x)]
              (if (> (count s) 20)
                (let [bd (BigDecimal. s)]
                  (.toString (.stripTrailingZeros bd)))
                s))))

        :cljs
        (let [d (m/->dec x)]
          (cond
            round
            (.toFixed d round)

            sig-figs
            (let [s (.toPrecision d sig-figs)]
              (if (str/includes? s ".")
                (-> s (str/replace #"0+$" "") (str/replace #"\.$" ""))
                s))

            (m/dinteger? d)
            (let [s (.toFixed d 0)]
              (if (> (count s) 20)
                (.toExponential d)
                s))

            :else
            (let [s (.toPrecision d 10)]
              (if (str/includes? s ".")
                (-> s
                    (str/replace #"0+$" "")
                    (str/replace #"\.$" ""))
                s))))))))

(defn format-error
  "Format an error map into a human-readable string (without 'Error: ' prefix)."
  [{:keys [error unit phrase]}]
  (case error
    :unknown-unit (str "Unknown unit: \"" unit "\"")
    :unparseable (str "Could not parse: \"" phrase "\"")
    :ambiguous-quantities "Both sides of the conversion have quantities"
    :incompatible-dimensions "Incompatible dimensions"
    :unsupported-operation "Unsupported operation"
    :invalid-request "Invalid request"
    (str "Error: " (pr-str {:error error}))))

(defn roll-preview
  "If input is a roll command, return a preview string like 'Rolling 2d6 ...'
   without actually rolling. Returns nil for non-roll input.
   Returns an error string if the dice expression is invalid."
  [input]
  (when-let [expr (dice/roll-input? input)]
    (let [parsed (dice/parse-dice expr)]
      (if (:error parsed)
        {:error (:message parsed)}
        {:result (str "Rolling " expr " ...")}))))

(defn- format-roll-result
  "Format a dice roll result for display."
  [{:keys [roll]}]
  (let [{:keys [rolls kept dropped modifier total comparison]} roll
        exploding? (and (seq rolls) (map? (first rolls)))
        rolls-str (if exploding?
                    (str/join ", " (for [{:keys [initial exploded total]} rolls]
                                    (if (seq exploded)
                                      (str initial "!" (str/join "!" exploded) "=" total)
                                      (str initial))))
                    (str/join ", " rolls))
        has-kd? (not= rolls kept)
        kept-str (when has-kd?
                   (str " -> kept [" (str/join ", " (sort > kept)) "]"
                        (when (seq dropped)
                          (str ", dropped [" (str/join ", " dropped) "]"))))
        mod-str (cond
                  (pos? modifier) (str " + " modifier)
                  (neg? modifier) (str " - " (- modifier))
                  :else "")
        cmp-str (when comparison
                  (str " " (name (:op comparison)) " " (:target comparison)
                       " -> " (if (:success comparison) "Success!" "Failure")))]
    (str "Rolls: [" rolls-str "]"
         kept-str
         mod-str
         " = " total
         cmp-str)))

(defn format-op-result
  "Format the result of a non-conversion op (:percentage, :root, :modulo, :tip, :tax, :roll).
   Returns a result string or nil if the op is not handled here."
  [parsed result fmt-opts]
  (case (:op parsed)
    :percentage (str (format-number (:value result) fmt-opts)
                     (:unit-label result))

    :root (format-number (:value result) fmt-opts)

    :modulo (format-number (:value result) fmt-opts)

    :trig (let [fn-name (name (:fn parsed))
                inverse? (#{:asin :acos :atan} (:fn parsed))
                ;; For forward trig, annotate input angle; for inverse, input is dimensionless
                input-str (if inverse?
                            (str fn-name " " (format-number (:value parsed) nil))
                            (let [angle-suffix (if (= :rad (:angle-mode parsed)) " rad" "°")]
                              (str fn-name " " (format-number (:value parsed) nil) angle-suffix)))
                result-str (str (format-number (:value result) fmt-opts)
                                (when (:unit-label result)
                                  (if (= "°" (:unit-label result))
                                    "°"
                                    (str " " (:unit-label result)))))]
            (str input-str " = " result-str))

    :tip (let [money-opts (assoc fmt-opts :round 2)
               rows (:rows result)
               fmt-money (fn [v] (format-number v money-opts))
               bill-str (fmt-money (:bill result))
               all-tips (map #(fmt-money (:tip %)) rows)
               all-totals (map #(fmt-money (:total %)) rows)
               labels (map :label rows)
               lpad (fn [s w] (let [n (- w (count s))]
                                (str (apply str (repeat n " ")) s)))
               max-label (apply max (count "Bill") (map count labels))
               dollar (fn [s w] (let [n (- w (count s))]
                                 (str (apply str (repeat n " ")) "$" s)))
               max-bill (apply max (count bill-str) (map count all-tips))
               max-total (apply max (map count all-totals))
               fmt-row (fn [{:keys [label]} tip-s total-s]
                         (str (lpad label max-label) ": "
                              "Tip " (dollar tip-s max-bill)
                              " -> Total " (dollar total-s max-total)))]
           (str (lpad "Bill" max-label) ": " (dollar bill-str max-bill) "\n"
                (str/join "\n" (map fmt-row rows all-tips all-totals))))

    :tax (let [money-opts (assoc fmt-opts :round 2)]
           (str "Price: $" (format-number (:price parsed) money-opts)
                ", Tax: $" (format-number (:tax result) money-opts)
                " (" (format-number (:percent parsed) fmt-opts) "%)"
                ", Total: $" (format-number (:total result) money-opts)))

    :roll (format-roll-result result)

    :base-convert (:value result)

    nil))
