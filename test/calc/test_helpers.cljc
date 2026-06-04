(ns calc.test-helpers
  "Cross-platform test utilities for calc tests."
  (:require [clojure.string :as str]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.format :as fmt]))

(defn evaluate
  "Cross-platform evaluate: parse input, convert, format result.
   Handles all ops including :tip, :tax, :root, :roll, etc.
   Returns {:result str} or {:error str}, with optional :target and :from."
  [input fmt-opts]
  (let [input (str/trim input)]
    (when-not (str/blank? input)
      (try
        (if-let [math-result (parser/parse-math input)]
          (if (map? math-result)
            {:result (str (:trig-expr math-result) " = "
                          (fmt/format-number (:math-value math-result) fmt-opts))}
            {:result (fmt/format-number math-result fmt-opts)})
          (let [parsed (parser/parse-request input)
                effective-fmt (merge (:format parsed) fmt-opts)
                result (ev/convert-request parsed)]
            (cond
              (not (:ok? result))
              {:error (fmt/format-error result)}

              (fmt/format-op-result parsed result effective-fmt)
              {:result (fmt/format-op-result parsed result effective-fmt)}

              (:mixed result)
              {:result (str/join " "
                                 (for [{:keys [value unit-label]} (:mixed result)]
                                   (str (fmt/format-number value effective-fmt) " " unit-label)))}

              (:unit-label result)
              {:result (str (fmt/format-number (:value result) effective-fmt)
                            " " (:unit-label result))}

              (= :auto (:to parsed))
              {:result (if (:value result)
                         (fmt/format-number (:value result) effective-fmt)
                         (str (:value result)))}

              :else
              {:result (fmt/format-number (:value result) effective-fmt)
               :target (ev/format-unit-label (:to parsed))})))
        (catch #?(:cljs :default :default Exception) e
          {:error (str #?(:cljs (.-message e) :default (ex-message e)))})))))

(defn approx==
  "Compare two numbers with a relative tolerance (default 1e-6).
   Useful for CLJS where float precision differs from JVM BigDecimal."
  ([a b] (approx== a b 1e-6))
  ([a b tol]
   (if (== a 0)
     (< (abs (double b)) tol)
     (< (abs (/ (- (double a) (double b)) (double a))) tol))))

(defn deep==
  "Recursively compare structures, using approx== for numbers.
   Handles BigDecimal/BigInt vs plain number differences across platforms,
   and tolerates minor float precision differences in CLJS."
  [a b]
  (cond
    (and (number? a) (number? b)) (approx== a b)
    (and (map? a) (map? b))
    (and (= (count a) (count b))
         (every? (fn [k] (and (contains? b k) (deep== (get a k) (get b k)))) (keys a)))
    (and (vector? a) (vector? b))
    (and (= (count a) (count b))
         (every? true? (map deep== a b)))
    (and (sequential? a) (sequential? b))
    (and (= (count a) (count b))
         (every? true? (map deep== a b)))
    :else (= a b)))
