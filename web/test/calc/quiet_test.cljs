(ns calc.quiet-test
  (:require [cljs.test :as t]
            ["fs" :as fs]))

;; Suppress "Testing calc.xxx-test" namespace headers
(defmethod t/report [:cljs.test/default :begin-test-ns] [_m])

;; Write structured results and suppress printed summary
(defmethod t/report [:cljs.test/default :summary] [m]
  (fs/writeFileSync "../.test-results/cljs.edn"
    (pr-str {:platform "ClojureScript"
             :test (:test m 0)
             :pass (:pass m 0)
             :fail (:fail m 0)
             :error (:error m 0)})))
