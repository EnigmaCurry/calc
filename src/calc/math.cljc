(ns calc.math
  #?(:clj (:import [java.math BigDecimal MathContext RoundingMode]))
  #?(:cljs (:require ["decimal.js" :as Decimal])))

;; ============================================================================
;; Cross-platform arbitrary-precision arithmetic
;;
;; JVM:  delegates to native Clojure ops (BigDecimal/ratio aware)
;; CLJS: uses decimal.js for arbitrary precision
;; ============================================================================

;; ---------------------------------------------------------------------------
;; Configuration and precision context
;; ---------------------------------------------------------------------------

#?(:clj (def ^:private math-context MathContext/DECIMAL128))
#?(:clj (def ^:private output-scale 14))

#?(:cljs
   (do
     (set! (.-precision Decimal) 200)
     (set! (.-rounding Decimal) 4))) ;; ROUND_HALF_UP

;; ---------------------------------------------------------------------------
;; Construction / conversion
;; ---------------------------------------------------------------------------

(defn ->dec
  "Convert a number to BigDecimal on JVM, Decimal on CLJS."
  [x]
  #?(:clj
     (cond
       (instance? BigDecimal x) x
       (integer? x) (BigDecimal/valueOf (long x))
       (ratio? x)   (.divide (BigDecimal/valueOf (long (numerator x)))
                             (BigDecimal/valueOf (long (denominator x)))
                             math-context)
       :else         (BigDecimal. (str x)))
     :cljs
     (if (instance? Decimal x)
       x
       (Decimal. (if (string? x) x (str x))))))

(defn dec->double
  "Convert to a native double/number."
  [x]
  #?(:clj (double x)
     :cljs (if (instance? Decimal x) (.toNumber ^js x) (js/Number x))))

(defn dec->str
  "Convert a decimal to a plain string (no scientific notation)."
  [x]
  #?(:clj
     (cond
       (instance? BigDecimal x) (.toPlainString ^BigDecimal x)
       :else (str x))
     :cljs
     (if (instance? Decimal x) (.toFixed x) (str x))))

(defn dec-type?
  "True if x is a BigDecimal (JVM) or Decimal (CLJS)."
  [x]
  #?(:clj (instance? BigDecimal x)
     :cljs (instance? Decimal x)))

;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(defn d+
  "Add two numbers with full precision."
  [a b]
  #?(:clj (+ a b)
     :cljs (.plus (->dec a) (->dec b))))

(defn d-
  "Subtract b from a with full precision."
  ([a]
   #?(:clj (- a)
      :cljs (.negated (->dec a))))
  ([a b]
   #?(:clj (- a b)
      :cljs (.minus (->dec a) (->dec b)))))

(defn d*
  "Multiply two numbers with full precision."
  [a b]
  #?(:clj (* a b)
     :cljs (.times (->dec a) (->dec b))))

(defn ddiv
  "Divide a by b with full precision."
  [a b]
  #?(:clj
     (cond
       (or (instance? BigDecimal a)
           (instance? BigDecimal b)
           (ratio? a)
           (ratio? b))
       (.divide (->dec a) (->dec b) math-context)
       :else
       (/ a b))
     :cljs
     (.dividedBy (->dec a) (->dec b))))

(defn dmod
  "Modulo operation."
  [a b]
  #?(:clj (mod a b)
     :cljs (.modulo (->dec a) (->dec b))))

(defn dquot
  "Integer quotient (truncated division)."
  [a b]
  #?(:clj (quot a b)
     :cljs (.truncated ^js (.dividedBy (->dec a) (->dec b)))))

;; ---------------------------------------------------------------------------
;; Comparison
;; ---------------------------------------------------------------------------

(defn dcmp
  "Compare two numbers. Returns -1, 0, or 1."
  [a b]
  #?(:clj (compare a b)
     :cljs (.comparedTo (->dec a) (->dec b))))

(defn d> [a b]
  #?(:clj (> a b)
     :cljs (.greaterThan (->dec a) (->dec b))))

(defn d< [a b]
  #?(:clj (< a b)
     :cljs (.lessThan (->dec a) (->dec b))))

(defn d>= [a b]
  #?(:clj (>= a b)
     :cljs (.greaterThanOrEqualTo (->dec a) (->dec b))))

(defn d<= [a b]
  #?(:clj (<= a b)
     :cljs (.lessThanOrEqualTo (->dec a) (->dec b))))

(defn d== [a b]
  #?(:clj (== a b)
     :cljs (.equals (->dec a) (->dec b))))

(defn dzero? [x]
  #?(:clj (zero? x)
     :cljs (.isZero (->dec x))))

(defn dpos? [x]
  #?(:clj (pos? x)
     :cljs (.isPositive (->dec x))))

(defn dneg? [x]
  #?(:clj (neg? x)
     :cljs (.isNegative (->dec x))))

(defn dinteger? [x]
  #?(:clj (or (integer? x)
              (and (instance? BigDecimal x)
                   (try (.toBigIntegerExact x) true
                        (catch ArithmeticException _ false))))
     :cljs (.isInteger (->dec x))))

;; ---------------------------------------------------------------------------
;; Math functions
;; ---------------------------------------------------------------------------

(defn dabs [x]
  #?(:clj (if (instance? BigDecimal x) (.abs x) (Math/abs (double x)))
     :cljs (.abs (->dec x))))

(defn dpow [base exp]
  #?(:clj (Math/pow (double base) (double exp))
     :cljs (.pow (->dec base) (->dec exp))))

(defn dsqrt [x]
  #?(:clj (Math/sqrt (double x))
     :cljs (.sqrt (->dec x))))

(defn dfloor [x]
  #?(:clj (Math/floor (double x))
     :cljs (.floor (->dec x))))

(defn dceil [x]
  #?(:clj (Math/ceil (double x))
     :cljs (.ceil (->dec x))))

(defn dround [x]
  #?(:clj (Math/round (double x))
     :cljs (.round (->dec x))))

(defn dlog10 [x]
  #?(:clj (Math/log10 (double x))
     :cljs (let [^js r (.log (->dec x) 10)] (.toNumber r))))

;; ---------------------------------------------------------------------------
;; Trig functions
;; ---------------------------------------------------------------------------

(defn dsin [x]
  #?(:clj (Math/sin (double x))
     :cljs (let [^js r (.sin (->dec x))] (.toNumber r))))

(defn dcos [x]
  #?(:clj (Math/cos (double x))
     :cljs (let [^js r (.cos (->dec x))] (.toNumber r))))

(defn dtan [x]
  #?(:clj (Math/tan (double x))
     :cljs (let [^js r (.tan (->dec x))] (.toNumber r))))

(defn dasin [x]
  #?(:clj (Math/asin (double x))
     :cljs (let [^js r (.asin (->dec x))] (.toNumber r))))

(defn dacos [x]
  #?(:clj (Math/acos (double x))
     :cljs (let [^js r (.acos (->dec x))] (.toNumber r))))

(defn datan [x]
  #?(:clj (Math/atan (double x))
     :cljs (let [^js r (.atan (->dec x))] (.toNumber r))))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def PI
  #?(:clj Math/PI
     :cljs (let [^js r (.acos (->dec -1))] (.toNumber r))))

(def E
  #?(:clj Math/E
     :cljs (let [^js r (.exp (->dec 1))] (.toNumber r))))

;; ---------------------------------------------------------------------------
;; Normalization
;; ---------------------------------------------------------------------------

(defn normalize
  "Normalize a computed value: strip trailing zeros, convert integers to ints."
  [x]
  #?(:clj
     (cond
       (integer? x)
       x

       (ratio? x)
       (if (= 1 (denominator x))
         (bigint (numerator x))
         x)

       (instance? BigDecimal x)
       (let [stripped (.stripTrailingZeros x)]
         (try
           (bigint (.toBigIntegerExact stripped))
           (catch ArithmeticException _
             (.stripTrailingZeros
              (.setScale x output-scale RoundingMode/HALF_UP)))))

       :else
       x)
     :cljs
     (let [^js d (->dec x)]
       (if (.isInteger d)
         (.toNumber d)
         (let [^js s (.toSignificantDigits d 12)]
           (.toNumber s))))))
