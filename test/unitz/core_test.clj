(ns unitz.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [unitz.core :as u]))

(deftest unit-lookup-test
  (testing "known units resolve to metadata"
    (is (= {:dimension {:length 1}
            :scale 381/1250}
           (u/unit :ft))))

  (testing "unknown units throw"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown unit"
         (u/unit :wat)))))

(deftest compatibility-test
  (testing "simple compatible units"
    (is (true? (u/compatible? :ft :yd))))

  (testing "simple incompatible units"
    (is (false? (u/compatible? :ft :s))))

  (testing "compound compatible units"
    (is (true? (u/compatible? [:/ :mile :hour]
                              [:/ :ft :s]))))

  (testing "compound incompatible units"
    (is (false? (u/compatible? [:/ :mile :hour]
                               :ft)))))

(deftest simple-conversion-test
  (testing "feet to yards"
    (is (= 4N
           (u/convert 12 :ft :yd))))

  (testing "hours to minutes"
    (is (= 60
           (u/convert 1 :hour :min)))))

(deftest compound-conversion-test
  (testing "miles per hour to feet per second"
    (is (= 88N
           (u/convert 60 [:/ :mile :hour] [:/ :ft :s]))))

  (testing "square yards to square feet"
    (is (= 9N
           (u/convert 1 [:* :yd :yd] [:* :ft :ft])))))

(deftest exponent-helper-test
  (testing "zero exponents are removed"
    (is (= {:length 2}
           (u/clean-exponents {:length 2 :time 0}))))

  (testing "exponents merge by addition"
    (is (= {:length 1 :time -1}
           (u/merge-exponents {:length 1}
                              {:time -1}))))

  (testing "opposite exponents cancel out"
    (is (= {}
           (u/merge-exponents {:length 1}
                              {:length -1})))))

(deftest unit-algebra-test
  (testing "dividing meters by seconds creates length over time"
    (is (= {:dimension {:length 1 :time -1}
            :scale 1}
           (u/divide-units (u/unit :m)
                           (u/unit :s)))))

  (testing "feet per second has expected resolved unit"
    (is (= {:dimension {:length 1 :time -1}
            :scale 381/1250}
           (u/divide-units (u/unit :ft)
                           (u/unit :s)))))

  (testing "miles per hour has expected resolved unit"
    (is (= {:dimension {:length 1 :time -1}
            :scale 1397/3125}
           (u/divide-units (u/unit :mile)
                           (u/unit :hour))))))

(deftest resolved-unit-conversion-test
  (testing "convert between already-resolved compound units"
    (let [ft-per-second (u/divide-units (u/unit :ft)
                                        (u/unit :s))
          miles-per-hour (u/divide-units (u/unit :mile)
                                         (u/unit :hour))]
      (is (= 88N
             (u/convert-resolved-units 60
                                       miles-per-hour
                                       ft-per-second))))))

(deftest invalid-conversion-test
  (testing "incompatible conversions throw"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Incompatible units"
         (u/convert 1 :ft :s)))))

(deftest invalid-unit-expression-test
  (testing "invalid expression operator throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown unit expression operator"
         (u/unit-expr [:+ :ft :s]))))

  (testing "invalid expression type throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid unit expression"
         (u/unit-expr "ft")))))
