(ns calc.shared-test
  "Cross-platform tests driven by calc.test-cases.
   Runs on Babashka, JVM Clojure, and ClojureScript (browser via shadow-cljs)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.format :as fmt]
            [calc.test-cases :as tc]))

(defn evaluate
  "Portable evaluate: parse input, convert, format result.
   Returns {:result str} or {:error str}."
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

              (:unit-label result)
              {:result (fmt/format-number (:value result) effective-fmt)
               :target (:unit-label result)}

              (= :auto (:to parsed))
              {:result (fmt/format-number (:value result) effective-fmt)}

              :else
              {:result (fmt/format-number (:value result) effective-fmt)
               :target (ev/format-unit-label (:to parsed))})))
        (catch #?(:clj Exception :cljs :default) e
          {:error (str #?(:clj (.getMessage e) :cljs (.-message e)))})))))

(defn run-case [{:keys [input result target error]}]
  (let [ev (evaluate input nil)]
    (if error
      (is (some? (:error ev))
          (str "Expected error for: " (pr-str input)))
      (do
        (is (nil? (:error ev))
            (str "Unexpected error for: " (pr-str input) " → " (:error ev)))
        (when result
          (is (str/includes? (str (:result ev)) result)
              (str "For " (pr-str input)
                   ": expected result containing " (pr-str result)
                   ", got " (pr-str (:result ev)))))
        (when target
          (is (some-> (:target ev) (str/includes? target))
              (str "For " (pr-str input)
                   ": expected target containing " (pr-str target)
                   ", got " (pr-str (:target ev)))))))))

(deftest shared-conversion-tests
  (doseq [[group cases] (group-by :group tc/conversion-cases)]
    (testing group
      (doseq [c cases] (run-case c)))))

(deftest shared-math-tests
  (doseq [[group cases] (group-by :group tc/math-cases)]
    (testing group
      (doseq [c cases] (run-case c)))))

(deftest shared-percentage-tests
  (doseq [[group cases] (group-by :group tc/percentage-cases)]
    (testing group
      (doseq [c cases] (run-case c)))))

(deftest shared-formatting-tests
  (doseq [[group cases] (group-by :group tc/formatting-cases)]
    (testing group
      (doseq [c cases] (run-case c)))))

(deftest shared-error-tests
  (doseq [[group cases] (group-by :group tc/error-cases)]
    (testing group
      (doseq [c cases] (run-case c)))))
