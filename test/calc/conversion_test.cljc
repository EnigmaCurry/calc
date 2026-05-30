(ns calc.conversion-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.units :as u]
            [calc.eval :as ev]))

(deftest compatibility-test
  (testing "simple compatible units"
    (is (true? (u/compatible? :ft :yd))))

  (testing "simple incompatible units"
    (is (false? (u/compatible? :ft :s))))

  (testing "compound compatible units"
    (is (true? (u/compatible? [:/ :mi :hr]
                              [:/ :ft :s]))))

  (testing "compound incompatible units"
    (is (false? (u/compatible? [:/ :mi :hr]
                               :ft)))))

(deftest simple-conversion-test
  (testing "feet to yards"
    (is (== 4 (ev/convert-scalar 12 :ft :yd))))

  (testing "hours to minutes"
    (is (== 60 (ev/convert-scalar 1 :hr :min)))))

(deftest compound-conversion-test
  (testing "miles per hour to feet per second"
    (is (== 88 (ev/convert-scalar 60 [:/ :mi :hr] [:/ :ft :s]))))

  (testing "square yards to square feet"
    (is (== 9 (ev/convert-scalar 1 [:* :yd :yd] [:* :ft :ft])))))

(deftest invalid-conversion-test
  (testing "incompatible conversions return error"
    (is (= :incompatible-dimensions
           (:error (ev/convert-scalar 1 :ft :s))))))

(deftest length-conversion-test
  (testing "feet to yards"
    (is (== 4 (ev/convert-scalar 12 :ft :yd))))

  (testing "miles to feet"
    (is (== 5280 (ev/convert-scalar 1 :mi :ft)))))

(deftest mass-conversion-test
  (testing "kilograms to grams"
    (is (== 1000 (ev/convert-scalar 1 :kg :g))))

  (testing "pounds to ounces"
    (is (== 16 (ev/convert-scalar 1 :lb :oz)))))

(deftest time-conversion-test
  (testing "hours to minutes"
    (is (== 60 (ev/convert-scalar 1 :hr :min))))

  (testing "days to hours"
    (is (== 24 (ev/convert-scalar 1 :day :hr)))))

(deftest area-conversion-test
  (testing "square yards to square feet"
    (is (== 9 (ev/convert-scalar 1 [:* :yd :yd] [:* :ft :ft])))))

(deftest volume-conversion-test
  (testing "liters to milliliters"
    (is (== 1000 (ev/convert-scalar 1 :l :ml))))

  (testing "milliliters to liters"
    (is (== 0.001 (ev/convert-scalar 1 :ml :l)))))

(deftest speed-conversion-test
  (testing "miles per hour to feet per second"
    (is (== 88 (ev/convert-scalar 60 [:/ :mi :hr] [:/ :ft :s])))))
