(ns unitz.core)

(def units
  {;; -------------------------
   ;; Length
   ;; -------------------------

   :m    {:kind      :length
          :dimension {:length 1}
          :factor    1}

   :ft   {:kind      :length
          :dimension {:length 1}
          :factor    381/1250}

   :yd   {:kind      :length
          :dimension {:length 1}
          :factor    1143/1250}

   :mile {:kind      :length
          :dimension {:length 1}
          :factor    201168/125}

   ;; -------------------------
   ;; Area
   ;; -------------------------
   ;;
   ;; Area is not a base dimension.
   ;; It is length^2.

   :m2   {:kind      :area
          :dimension {:length 2}
          :factor    1}

   :ft2  {:kind      :area
          :dimension {:length 2}
          :factor    (* 381/1250 381/1250)}

   :acre {:kind      :area
          :dimension {:length 2}
          :factor    40468564224/10000000}

   ;; -------------------------
   ;; Volume
   ;; -------------------------
   ;;
   ;; Volume is length^3.

   :m3    {:kind      :volume
           :dimension {:length 3}
           :factor    1}

   :liter {:kind      :volume
           :dimension {:length 3}
           :factor    1/1000}

   :L     {:kind      :volume
           :dimension {:length 3}
           :factor    1/1000}

   :ml    {:kind      :volume
           :dimension {:length 3}
           :factor    1/1000000}

   :gal   {:kind      :volume
           :dimension {:length 3}
           :factor    473176473/125000000000}

   ;; -------------------------
   ;; Mass
   ;; -------------------------

   :kg   {:kind      :mass
          :dimension {:mass 1}
          :factor    1}

   :g    {:kind      :mass
          :dimension {:mass 1}
          :factor    1/1000}

   :lb   {:kind      :mass
          :dimension {:mass 1}
          :factor    45359237/100000000}

   :oz   {:kind      :mass
          :dimension {:mass 1}
          :factor    45359237/1600000000}

   ;; -------------------------
   ;; Time
   ;; -------------------------

   :s    {:kind      :time
          :dimension {:time 1}
          :factor    1}

   :min  {:kind      :time
          :dimension {:time 1}
          :factor    60}

   :hour {:kind      :time
          :dimension {:time 1}
          :factor    3600}

   :day  {:kind      :time
          :dimension {:time 1}
          :factor    86400}

   ;; -------------------------
   ;; Speed
   ;; -------------------------
   ;;
   ;; Speed is length / time.

   :mps  {:kind      :speed
          :dimension {:length 1
                      :time -1}
          :factor    1}

   :fps  {:kind      :speed
          :dimension {:length 1
                      :time -1}
          :factor    381/1250}

   :mph  {:kind      :speed
          :dimension {:length 1
                      :time -1}
          :factor    (/ 201168/125 3600)}

   ;; -------------------------
   ;; Acceleration
   ;; -------------------------
   ;;
   ;; Acceleration is length / time^2.

   :mps2 {:kind      :acceleration
          :dimension {:length 1
                      :time -2}
          :factor    1}

   ;; -------------------------
   ;; Force
   ;; -------------------------
   ;;
   ;; Newton = kg*m/s^2

   :N    {:kind      :force
          :dimension {:mass 1
                      :length 1
                      :time -2}
          :factor    1}

   ;; -------------------------
   ;; Energy
   ;; -------------------------
   ;;
   ;; Joule = kg*m^2/s^2

   :J    {:kind      :energy
          :dimension {:mass 1
                      :length 2
                      :time -2}
          :factor    1}

   ;; -------------------------
   ;; Power
   ;; -------------------------
   ;;
   ;; Watt = kg*m^2/s^3

   :W    {:kind      :power
          :dimension {:mass 1
                      :length 2
                      :time -3}
          :factor    1}

   ;; -------------------------
   ;; Electric current
   ;; -------------------------

   :A    {:kind      :electric-current
          :dimension {:current 1}
          :factor    1}

   ;; -------------------------
   ;; Temperature
   ;; -------------------------
   ;;
   ;; For now, only Kelvin works cleanly with factor-only conversion.
   ;; Fahrenheit and Celsius need offsets, so add them later.

   :K    {:kind      :temperature
          :dimension {:temperature 1}
          :factor    1}

   ;; -------------------------
   ;; Amount of substance
   ;; -------------------------

   :mol  {:kind      :amount
          :dimension {:amount 1}
          :factor    1}

   ;; -------------------------
   ;; Luminous intensity
   ;; -------------------------

   :cd   {:kind      :luminous-intensity
          :dimension {:luminous 1}
          :factor    1}})

(defn unit
  "Look up unit metadata for unit keyword `u`.

  Returns a map containing at least:

  - `:dimension` — a dimensional exponent map, such as `{:length 1}`
  - `:factor` — the scale factor relative to the base unit for that dimension

  Throws an `ex-info` exception if `u` is unknown.

  Example:

      (unit :ft)
      ;; => {:dimension {:length 1}, :factor 381/1250}"
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
   :factor (/ 1 (:factor u))})

(defn multiply-units
  "Multiply one or more unit metadata maps.

  Dimensions are added.
  Scales are multiplied."
  [& us]
  {:dimension (apply merge-exponents (map :dimension us))
   :factor (apply * (map :factor us))})

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
  (/ (* value (:factor from))
     (:factor to)))

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
