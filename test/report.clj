(require '[babashka.fs :as fs]
         '[clojure.edn :as edn])

(def results-dir ".test-results")

(def results
  (->> (fs/glob results-dir "*.edn")
       (map (comp edn/read-string slurp str))
       (sort-by #(case (:platform %) "Babashka" 0 "JVM" 1 "ClojureScript" 2 3))))

(when (empty? results)
  (println "No test results found.")
  (System/exit 1))

(let [totals (reduce (fn [acc r]
                       (merge-with + acc (select-keys r [:test :pass :fail :error])))
                     {:test 0 :pass 0 :fail 0 :error 0}
                     results)
      any-failures? (pos? (+ (:fail totals) (:error totals)))]

  (println)
  (println (format "  %-16s %6s %6s %6s %6s" "Platform" "Tests" "Pass" "Fail" "Error"))
  (println (apply str (repeat 48 "-")))
  (doseq [{:keys [platform test pass fail error]} results]
    (println (format "  %-16s %6d %6d %6d %6d" platform test pass fail error)))
  (println (apply str (repeat 48 "-")))
  (println (format "  %-16s %6d %6d %6d %6d" "Total" (:test totals) (:pass totals) (:fail totals) (:error totals)))
  (println)

  (when any-failures?
    (System/exit 1)))
