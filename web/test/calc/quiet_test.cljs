(ns calc.quiet-test
  (:require [cljs.test :as t]))

;; Suppress "Testing calc.xxx-test" namespace headers from test output
(defmethod t/report [:cljs.test/default :begin-test-ns] [_m])
