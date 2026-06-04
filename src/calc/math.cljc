(ns calc.math
  #?(:clj (:import [java.math BigDecimal MathContext RoundingMode]))
  #?(:cljs (:require ["decimal.js" :as Decimal]))
  #?(:lpy (:import decimal math)))

;; ============================================================================
;; Cross-platform arbitrary-precision arithmetic
;;
;; JVM:  delegates to native Clojure ops (BigDecimal/ratio aware)
;; CLJS: uses decimal.js for arbitrary precision
;; Basilisp: uses Python's decimal.Decimal and math modules
;; ============================================================================

;; ---------------------------------------------------------------------------
;; Configuration and precision context
;; ---------------------------------------------------------------------------

#?(:clj (def ^:private math-context MathContext/DECIMAL128))
#?(:clj (def ^:private output-scale 14))

#?(:lpy (do
          (set! (.-prec (decimal/getcontext)) 34)
          (def ^:private output-scale 14)))

(def default-precision 200)

#?(:cljs
   (do
     (set! (.-precision Decimal) default-precision)
     (set! (.-rounding Decimal) 4))) ;; ROUND_HALF_UP

(defn get-precision []
  #?(:cljs (.-precision Decimal)
     :default 34))

(defn set-precision! [n]
  #?(:cljs (set! (.-precision Decimal) n)
     :default nil))

;; ---------------------------------------------------------------------------
;; Construction / conversion
;; ---------------------------------------------------------------------------

(defn ->dec
  "Convert a number to BigDecimal on JVM, Decimal on CLJS, decimal.Decimal on Basilisp."
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
       (Decimal. (if (string? x) x (str x))))
     :lpy
     (cond
       (instance? decimal/Decimal x) x
       (integer? x) (decimal/Decimal (str x))
       (ratio? x)   (/ (decimal/Decimal (str (numerator x)))
                       (decimal/Decimal (str (denominator x))))
       :else         (decimal/Decimal (str x)))))

(defn dec->double
  "Convert to a native double/number."
  [x]
  #?(:cljs (if (instance? Decimal x) (.toNumber ^js x) (js/Number x))
     :default (double x)))

(defn plain-decimal-str
  "Convert a decimal to a plain string (no scientific notation). Basilisp helper."
  [x]
  #?(:lpy
     (let [s (str x)]
       (if (or (.find s "E") (.find s "e"))
         (python/format x "f")
         s))
     :default (str x)))

(defn dec->str
  "Convert a decimal to a plain string (no scientific notation)."
  [x]
  #?(:clj
     (cond
       (instance? BigDecimal x) (.toPlainString ^BigDecimal x)
       :else (str x))
     :cljs
     (if (instance? Decimal x) (.toFixed x) (str x))
     :lpy
     (if (instance? decimal/Decimal x)
       (plain-decimal-str x)
       (str x))))

(defn dec-type?
  "True if x is a BigDecimal (JVM), Decimal (CLJS), or decimal.Decimal (Basilisp)."
  [x]
  #?(:clj (instance? BigDecimal x)
     :cljs (instance? Decimal x)
     :lpy (instance? decimal/Decimal x)))

;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(defn d+
  "Add two numbers with full precision."
  [a b]
  #?(:cljs (.plus (->dec a) (->dec b))
     :default (+ a b)))

(defn d-
  "Subtract b from a with full precision."
  ([a]
   #?(:cljs (.negated (->dec a))
      :default (- a)))
  ([a b]
   #?(:cljs (.minus (->dec a) (->dec b))
      :default (- a b))))

(defn d*
  "Multiply two numbers with full precision."
  [a b]
  #?(:cljs (.times (->dec a) (->dec b))
     :default (* a b)))

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
     (.dividedBy (->dec a) (->dec b))
     :lpy
     (cond
       (or (instance? decimal/Decimal a)
           (instance? decimal/Decimal b)
           (ratio? a)
           (ratio? b))
       (/ (->dec a) (->dec b))
       :else
       (/ a b))))

(defn dmod
  "Modulo operation."
  [a b]
  #?(:cljs (.modulo (->dec a) (->dec b))
     :default (mod a b)))

(defn dquot
  "Integer quotient (truncated division)."
  [a b]
  #?(:cljs (.truncated ^js (.dividedBy (->dec a) (->dec b)))
     :default (quot a b)))

;; ---------------------------------------------------------------------------
;; Comparison
;; ---------------------------------------------------------------------------

(defn dcmp
  "Compare two numbers. Returns -1, 0, or 1."
  [a b]
  #?(:cljs (.comparedTo (->dec a) (->dec b))
     :default (compare a b)))

(defn d> [a b]
  #?(:cljs (.greaterThan (->dec a) (->dec b))
     :default (> a b)))

(defn d< [a b]
  #?(:cljs (.lessThan (->dec a) (->dec b))
     :default (< a b)))

(defn d>= [a b]
  #?(:cljs (.greaterThanOrEqualTo (->dec a) (->dec b))
     :default (>= a b)))

(defn d<= [a b]
  #?(:cljs (.lessThanOrEqualTo (->dec a) (->dec b))
     :default (<= a b)))

(defn d== [a b]
  #?(:cljs (.equals (->dec a) (->dec b))
     :default (== a b)))

(defn dzero? [x]
  #?(:cljs (.isZero (->dec x))
     :default (zero? x)))

(defn dpos? [x]
  #?(:cljs (.isPositive (->dec x))
     :default (pos? x)))

(defn dneg? [x]
  #?(:cljs (.isNegative (->dec x))
     :default (neg? x)))

(defn dinteger? [x]
  #?(:clj (or (integer? x)
              (and (instance? BigDecimal x)
                   (try (.toBigIntegerExact x) true
                        (catch ArithmeticException _ false))))
     :cljs (.isInteger (->dec x))
     :lpy (or (integer? x)
              (and (instance? decimal/Decimal x)
                   (= (.to-integral-value x) x)))))

;; ---------------------------------------------------------------------------
;; Math functions
;; ---------------------------------------------------------------------------

(defn dabs [x]
  #?(:clj (if (instance? BigDecimal x) (.abs x) (Math/abs (double x)))
     :cljs (.abs (->dec x))
     :lpy (abs x)))

(defn dpow [base exp]
  #?(:clj (Math/pow (double base) (double exp))
     :cljs (.pow (->dec base) (->dec exp))
     :lpy (math/pow (double base) (double exp))))

(defn dsqrt [x]
  #?(:clj (Math/sqrt (double x))
     :cljs (js/Math.sqrt (dec->double x))
     :lpy (math/sqrt (double x))))

(defn dfloor [x]
  #?(:clj (Math/floor (double x))
     :cljs (.floor (->dec x))
     :lpy (double (math/floor (double x)))))

(defn dceil [x]
  #?(:clj (Math/ceil (double x))
     :cljs (.ceil (->dec x))
     :lpy (double (math/ceil (double x)))))

(defn dround [x]
  #?(:clj (Math/round (double x))
     :cljs (.round (->dec x))
     :lpy (long (round (double x)))))

(defn dlog10 [x]
  #?(:clj (Math/log10 (double x))
     :cljs (js/Math.log10 (dec->double x))
     :lpy (math/log10 (double x))))

;; ---------------------------------------------------------------------------
;; Trig functions
;; ---------------------------------------------------------------------------

(defn dsin [x]
  #?(:clj (Math/sin (double x))
     :cljs (js/Math.sin (dec->double x))
     :lpy (math/sin (double x))))

(defn dcos [x]
  #?(:clj (Math/cos (double x))
     :cljs (js/Math.cos (dec->double x))
     :lpy (math/cos (double x))))

(defn dtan [x]
  #?(:clj (Math/tan (double x))
     :cljs (js/Math.tan (dec->double x))
     :lpy (math/tan (double x))))

(defn dasin [x]
  #?(:clj (Math/asin (double x))
     :cljs (js/Math.asin (dec->double x))
     :lpy (math/asin (double x))))

(defn dacos [x]
  #?(:clj (Math/acos (double x))
     :cljs (js/Math.acos (dec->double x))
     :lpy (math/acos (double x))))

(defn datan [x]
  #?(:clj (Math/atan (double x))
     :cljs (js/Math.atan (dec->double x))
     :lpy (math/atan (double x))))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def PI
  #?(:clj Math/PI
     :cljs js/Math.PI
     :lpy math/pi))

(def E
  #?(:clj Math/E
     :cljs js/Math.E
     :lpy math/e))

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
     (let [^js d (->dec x)
           n (.toNumber d)]
       (if (js/isFinite n)
         (if (.isInteger d)
           n
           (let [^js s (.toSignificantDigits d 12)]
             (.toNumber s)))
         (.toString d)))
     :lpy
     (cond
       (integer? x)
       x

       (ratio? x)
       (if (= 1 (denominator x))
         (bigint (numerator x))
         x)

       (instance? decimal/Decimal x)
       (let [stripped (.normalize x)]
         (if (= (.to-integral-value stripped) stripped)
           (int stripped)
           (.normalize
            (.quantize x (decimal/Decimal (str "1E-" output-scale))
                       decimal/ROUND-HALF-UP))))

       :else
       x)))
