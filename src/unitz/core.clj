(ns unitz.core)

(def units
  {;; Length
   :m    {:dimension {:length 1}
          :scale     1}
   :ft   {:dimension {:length 1}
          :scale     381/1250}
   :yd   {:dimension {:length 1}
          :scale     1143/1250}
   :mile {:dimension {:length 1}
          :scale     201168/125}

   ;; Time
   :s    {:dimension {:time 1}
          :scale     1}
   :min  {:dimension {:time 1}
          :scale     60}
   :hour {:dimension {:time 1}
          :scale     3600}})

(defn unit
  "Look up unit metadata for unit keyword `u`.

  Returns a map containing at least:

  - `:dimension` — a dimensional exponent map, such as `{:length 1}`
  - `:scale` — the scale factor relative to the base unit for that dimension

  Throws an `ex-info` exception if `u` is unknown.

  Example:

      (unit :ft)
      ;; => {:dimension {:length 1}, :scale 381/1250}"
  [u]
  (or (get units u)
      (throw (ex-info "Unknown unit" {:unit u}))))

(defn clean-exponents
  "Remove zero-valued exponents from dimensional exponent map `m`.

  This keeps dimension maps canonical by dropping dimensions that cancel out.

  Examples:

      (clean-exponents {:length 2 :time 0})
      ;; => {:length 2}

      (clean-exponents {:length 1 :time -1})
      ;; => {:length 1, :time -1}"
  [m]
  (->> m
       (remove (fn [[_ exponent]]
                 (zero? exponent)))
       (into {})))

(defn merge-exponents
  "Merge one or more dimensional exponent maps by adding matching exponents.

  Dimensions with a resulting exponent of zero are removed.

  This is useful for combining compound units, such as multiplying or dividing
  dimensions.

  Examples:

      (merge-exponents {:length 1} {:time -1})
      ;; => {:length 1, :time -1}

      (merge-exponents {:length 1} {:length -1})
      ;; => {}"
  [& maps]
  (clean-exponents
   (apply merge-with + maps)))

(defn invert-unit
  "Invert a unit metadata map.

  Example:
      meter    => length
      per-sec  => time^-1"
  [u]
  {:dimension (update-vals (:dimension u) -)
   :scale (/ 1 (:scale u))})

(defn multiply-units
  "Multiply one or more unit metadata maps.

  Dimensions are added.
  Scales are multiplied."
  [& us]
  {:dimension (apply merge-exponents (map :dimension us))
   :scale (apply * (map :scale us))})

(defn divide-units
  "Divide unit metadata map `a` by unit metadata map `b`."
  [a b]
  (multiply-units a (invert-unit b)))

(defn compatible-resolved-units?
  "Return true if two resolved unit metadata maps have the same dimension."
  [from to]
  (= (:dimension from)
     (:dimension to)))

(defn convert-resolved-units
  "Convert numeric `value` between two resolved unit metadata maps.

  This is the lower-level conversion function used by `convert`.
  Callers usually want `convert`, which accepts unit expressions."
  [value from to]
  (when-not (compatible-resolved-units? from to)
    (throw (ex-info "Incompatible units"
                    {:from-dimension (:dimension from)
                     :to-dimension (:dimension to)})))
  (/ (* value (:scale from))
     (:scale to)))

(defn unit-expr
  "Resolve a unit expression into a unit metadata map.

  Supported forms:

      :ft
      [:/ :mile :hour]
      [:* :ft :ft]

  Examples:

      (unit-expr :ft)

      (unit-expr [:/ :mile :hour])
      ;; miles per hour

      (unit-expr [:* :ft :ft])
      ;; square feet"
  [expr]
  (cond
    (keyword? expr)
    (unit expr)

    (vector? expr)
    (let [[op a b] expr]
      (case op
        :* (multiply-units (unit-expr a)
                           (unit-expr b))

        :/ (divide-units (unit-expr a)
                         (unit-expr b))

        (throw (ex-info "Unknown unit expression operator"
                        {:operator op
                         :expr expr}))))

    :else
    (throw (ex-info "Invalid unit expression"
                    {:expr expr}))))

(defn compatible?
  "Return true if unit expressions `from` and `to` have the same dimension."
  [from to]
  (compatible-resolved-units? (unit-expr from)
                              (unit-expr to)))

(defn convert
  "Convert numeric `value` from unit expression `from` to unit expression `to`.

  Unit expressions may be simple unit keywords:

      :ft
      :yd
      :hour

  Or compound unit expressions:

      [:/ :mile :hour]
      [:/ :ft :s]
      [:* :ft :ft]

  Examples:

      (convert 12 :ft :yd)
      ;; => 4N

      (convert 60 [:/ :mile :hour] [:/ :ft :s])
      ;; => 88N

      (convert 1 [:* :yd :yd] [:* :ft :ft])
      ;; => 9N"
  [value from to]
  (convert-resolved-units value
                          (unit-expr from)
                          (unit-expr to)))

(comment
  (:scale (:ft units))
  ;;; Test user API:
  (unit :ft)
  (compatible? :ft :yd)
  (compatible? [:/ :mile :hour] [:/ :ft :s])
  (convert 12 :ft :yd)
  (convert 60 [:/ :mile :hour] [:/ :ft :s])
  (convert 1 [:* :yd :yd] [:* :ft :ft])

  ;;;Test internal:
  (clean-exponents {:length 2 :time 0})
  (merge-exponents {:length 1 :time -1})
  (divide-units (unit :m) (unit :s))
  (divide-units (unit :ft) (unit :s))
  (divide-units (unit :mile) (unit :hour))
  (def ft-per-second
    (divide-units (unit :ft) (unit :s)))
  (def miles-per-hour
    (divide-units (unit :mile) (unit :hour)))
  (convert-resolved-units 60 miles-per-hour ft-per-second))

