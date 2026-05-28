(require '[clojure.test :as test]
         '[unitz.core-test])

(let [{:keys [fail error]} (test/run-tests 'unitz.core-test)]
  (when (pos? (+ fail error))
    (System/exit 1)))
