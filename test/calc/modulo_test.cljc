(ns calc.modulo-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.test-helpers :as th]))

;; ==========================================================================
;; Parser tests — math expression syntax: %
;; ==========================================================================

(deftest parses-modulo-in-math-expressions
  (testing "% operator in math context"
    (are [expr expected] (th/deep== expected (parser/parse-math expr))
      "10%4"    2
      "10%3"    1
      "7%2"     1
      "100%7"   2
      "9%3"     0
      "15%4"    3)))

;; ==========================================================================
;; Parser tests — natural language modulo forms
;; ==========================================================================

(deftest parses-modulo-natural-language
  (testing "'X mod Y' form"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "10 mod 4"
      {:op :modulo :dividend 10 :divisor 4}

      "100 mod 7"
      {:op :modulo :dividend 100 :divisor 7}

      "9 mod 3"
      {:op :modulo :dividend 9 :divisor 3}))

  (testing "'X modulo Y' form"
    (is (th/deep== {:op :modulo :dividend 15 :divisor 4}
           (parser/parse-request "15 modulo 4"))))

  (testing "'what is X mod Y' form"
    (is (th/deep== {:op :modulo :dividend 10 :divisor 3}
           (parser/parse-request "what is 10 mod 3")))))

(deftest parses-modulo-with-percent-sign
  (testing "'X % Y' is parsed as modulo"
    (is (th/deep== {:op :modulo :dividend 10 :divisor 4}
           (parser/parse-request "10 % 4")))
    (is (th/deep== {:op :modulo :dividend 7 :divisor 2}
           (parser/parse-request "7 % 2")))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-modulo
  (testing "basic modulo operations"
    (are [dividend divisor expected]
      (let [r (ev/convert-request {:op :modulo :dividend dividend :divisor divisor})]
        (and (:ok? r) (== expected (:value r))))
      10 4 2
      10 3 1
      9  3 0
      7  2 1
      100 7 2
      15 4 3))

  (testing "modulo with decimals"
    (let [r (ev/convert-request {:op :modulo :dividend 10.5 :divisor 3})]
      (is (:ok? r))
      (is (== 1.5 (:value r)))))
)

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-modulo-math-expression
  (testing "% operator in math expression"
    (let [{:keys [result]} (th/evaluate "10 % 4" nil)]
      (is (= "2" result)))
    (let [{:keys [result]} (th/evaluate "10%3" nil)]
      (is (= "1" result)))
    (let [{:keys [result]} (th/evaluate "100 % 7" nil)]
      (is (= "2" result)))))

(deftest end-to-end-modulo-natural-language
  (testing "mod keyword"
    (let [{:keys [result]} (th/evaluate "10 mod 4" nil)]
      (is (= "2" result)))
    (let [{:keys [result]} (th/evaluate "10 mod 3" nil)]
      (is (= "1" result))))

  (testing "modulo keyword"
    (let [{:keys [result]} (th/evaluate "15 modulo 4" nil)]
      (is (= "3" result))))

  (testing "what is X mod Y"
    (let [{:keys [result]} (th/evaluate "what is 10 mod 3" nil)]
      (is (= "1" result)))))
