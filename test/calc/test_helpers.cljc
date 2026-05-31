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
          {:result (fmt/format-number math-result fmt-opts)}
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
        (catch #?(:clj Exception :cljs :default) e
          {:error (str #?(:clj (.getMessage e) :cljs (.-message e)))})))))

(defn deep==
  "Recursively compare structures, using == for numbers.
   Handles BigDecimal/BigInt vs plain number differences across platforms."
  [a b]
  (cond
    (and (number? a) (number? b)) (== a b)
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
