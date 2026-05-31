(ns test-runner-jvm
  (:require [clojure.test :as t]
            [clojure.string :as str]))

(def results-dir ".test-results")

(let [test-dir (java.io.File. "test")
      all-files (file-seq test-dir)
      test-nses (->> all-files
                     (filter #(.isFile %))
                     (map #(.getPath %))
                     (filter #(re-find #"_test\.cljc?$" %))
                     (map #(-> %
                               (str/replace #"^test/" "")
                               (str/replace #"\.cljc?$" "")
                               (str/replace "/" ".")
                               (str/replace "_" "-")
                               symbol))
                     sort)]
  (.mkdirs (java.io.File. results-dir))
  (doseq [ns-sym test-nses] (require ns-sym))
  (let [test-names (atom [])
        summary (with-redefs [t/report (let [orig (deref (var t/report))]
                                         (fn [m]
                                           (when (= :begin-test-var (:type m))
                                             (when-let [v (first t/*testing-vars*)]
                                               (swap! test-names conj
                                                      (str (ns-name (:ns (meta v)))
                                                           "/" (:name (meta v))))))
                                           (when-not (#{:begin-test-ns :summary} (:type m))
                                             (orig m))))]
                   (apply t/run-tests test-nses))]
    (spit (str results-dir "/jvm.edn")
          (pr-str {:platform "JVM"
                   :test (:test summary 0)
                   :pass (:pass summary 0)
                   :fail (:fail summary 0)
                   :error (:error summary 0)
                   :test-names (sort @test-names)}))
    (System/exit (if (pos? (+ (:fail summary 0) (:error summary 0))) 1 0))))
