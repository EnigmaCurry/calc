(require '[clojure.string :as str]
         '[clojure.test :as t]
         '[babashka.fs :as fs])

(def test-dir "test")
(def results-dir ".test-results")

(defn test-file? [path]
  (and (fs/regular-file? path)
       (re-find #"_test\.cljc?$" (str path))))

(defn path->namespace [path]
  (-> path
      str
      (str/replace #"^test/" "")
      (str/replace #"\.cljc?$" "")
      (str/replace #"/" ".")
      (str/replace #"_" "-")
      symbol))

(def test-namespaces
  (->> (concat (fs/glob test-dir "**/*_test.clj")
               (fs/glob test-dir "**/*_test.cljc"))
       (filter test-file?)
       (map path->namespace)
       sort
       vec))

(doseq [ns-sym test-namespaces]
  (require ns-sym))

(fs/create-dirs results-dir)

(def test-names (atom []))

(let [summary (with-redefs [t/report (let [orig-report t/report]
                                       (fn [m]
                                         (when (= :begin-test-var (:type m))
                                           (when-let [v (first t/*testing-vars*)]
                                             (swap! test-names conj
                                                    (str (ns-name (:ns (meta v)))
                                                         "/" (:name (meta v))))))
                                         (when-not (#{:begin-test-ns :summary} (:type m))
                                           (orig-report m))))]
                (apply t/run-tests test-namespaces))]
  (spit (str results-dir "/bb.edn")
        (pr-str {:platform "Babashka"
                 :test (:test summary 0)
                 :pass (:pass summary 0)
                 :fail (:fail summary 0)
                 :error (:error summary 0)
                 :test-names (sort @test-names)}))
  (when (pos? (+ (:fail summary 0) (:error summary 0)))
    (System/exit 1)))
