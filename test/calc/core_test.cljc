(ns calc.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.units :as u]))

(deftest unit-lookup-test
  (testing "known units resolve to a spec with dimension and scale"
    (let [ft-spec (u/unit-spec :ft)]
      (is (= {:length 1}
             (:dim ft-spec)))
      (is (pos? (:scale ft-spec)))))

  (testing "unknown units throw"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
         #"Unknown unit"
         (u/unit-spec :wat)))))

(deftest unit-algebra-test
  (testing "dividing meters by seconds creates length over time"
    (let [spec (u/unit-spec [:/ :m :s])]
      (is (= {:length 1 :time -1}
             (:dim spec)))))

  (testing "feet and seconds are compatible via division"
    (let [spec (u/unit-spec [:/ :ft :s])]
      (is (= {:length 1 :time -1}
             (:dim spec)))))

  (testing "miles per hour has expected dimension"
    (let [spec (u/unit-spec [:/ :mi :hr])]
      (is (= {:length 1 :time -1}
             (:dim spec))))))

(deftest invalid-unit-expression-test
  (testing "invalid expression form throws"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
         #"Invalid unit form"
         (u/unit-spec [:+ :ft :s]))))

  (testing "unknown unit keyword throws"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
         #"Unknown unit"
         (u/unit-spec :bogus)))))
