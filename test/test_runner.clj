(require '[clojure.string :as str]
         '[clojure.test :as t]
         '[babashka.fs :as fs])

(def test-dir "test")

(defn test-file? [path]
  (and (fs/regular-file? path)
       (str/ends-with? (str path) "_test.clj")))

(defn path->namespace [path]
  (-> path
      str
      (str/replace #"^test/" "")
      (str/replace #"\.clj$" "")
      (str/replace #"/" ".")
      (str/replace #"_" "-")
      symbol))

(def test-namespaces
  (->> (fs/glob test-dir "**/*_test.clj")
       (filter test-file?)
       (map path->namespace)
       sort
       vec))

(doseq [ns-sym test-namespaces]
  (require ns-sym))

(apply t/run-tests test-namespaces)
