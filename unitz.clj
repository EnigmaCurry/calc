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

(defn compatible?
  "Return true if units `from` and `to` have the same dimension.

  Compatible units can be converted directly with `convert`.

  Examples:

      (compatible? :ft :yd)
      ;; => true

      (compatible? :ft :s)
      ;; => false"
  [from to]
  (= (:dimension (unit from))
     (:dimension (unit to))))

(defn convert
  "Convert numeric `value` from unit `from` to unit `to`.

  The units must be dimensionally compatible. For example, feet can be
  converted to yards, but feet cannot be converted to seconds.

  Returns the converted value. Because many scale factors are ratios, results
  may be exact Clojure ratios instead of floating point numbers.

  Examples:

      (convert 12 :ft :yd)
      ;; => 4N

      (convert 1 :hour :min)
      ;; => 60

  Throws an `ex-info` exception if the units are incompatible."
  [value from to]
  (when-not (compatible? from to)
    (throw (ex-info "Incompatible units"
                    {:from from
                     :to to
                     :from-dimension (:dimension (unit from))
                     :to-dimension (:dimension (unit to))})))
  (/ (* value (:scale (unit from)))
     (:scale (unit to))))

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

(comment
  (:scale (:ft units))
  (unit :ft)
  (convert 12 :ft :yd)
  (clean-exponents {:length 2 :time 0})
  (merge-exponents {:length 1 :time -1}))
