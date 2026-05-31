(require '[clojure.string :as str]
         '[clojure.test :as t]
         '[babashka.fs :as fs])

(def test-dir "test")

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

(let [summary (with-redefs [t/report (let [orig-report t/report]
                                       (fn [m]
                                         (when-not (= :begin-test-ns (:type m))
                                           (orig-report m))))]
                (apply t/run-tests test-namespaces))]
  (when (pos? (+ (:fail summary 0) (:error summary 0)))
    (System/exit 1)))
