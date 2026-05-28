(ns calc.modulo-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.cli :as cli]))

;; ==========================================================================
;; Parser tests — math expression syntax: %
;; ==========================================================================

(deftest parses-modulo-in-math-expressions
  (testing "% operator in math context"
    (are [expr expected] (= expected (parser/parse-math expr))
      "10%4"    2N
      "10%3"    1N
      "7%2"     1N
      "100%7"   2N
      "9%3"     0N
      "15%4"    3N)))

;; ==========================================================================
;; Parser tests — natural language modulo forms
;; ==========================================================================

(deftest parses-modulo-natural-language
  (testing "'X mod Y' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "10 mod 4"
      {:op :modulo :dividend 10N :divisor 4N}

      "100 mod 7"
      {:op :modulo :dividend 100N :divisor 7N}

      "9 mod 3"
      {:op :modulo :dividend 9N :divisor 3N}))

  (testing "'X modulo Y' form"
    (is (= {:op :modulo :dividend 15N :divisor 4N}
           (parser/parse-request "15 modulo 4"))))

  (testing "'what is X mod Y' form"
    (is (= {:op :modulo :dividend 10N :divisor 3N}
           (parser/parse-request "what is 10 mod 3")))))

(deftest parses-modulo-with-percent-sign
  (testing "'X % Y' is parsed as modulo"
    (is (= {:op :modulo :dividend 10N :divisor 4N}
           (parser/parse-request "10 % 4")))
    (is (= {:op :modulo :dividend 7N :divisor 2N}
           (parser/parse-request "7 % 2")))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-modulo
  (testing "basic modulo operations"
    (are [dividend divisor expected]
      (let [r (ev/convert-request {:op :modulo :dividend dividend :divisor divisor})]
        (and (:ok? r) (= expected (:value r))))
      10N 4N 2N
      10N 3N 1N
      9N  3N 0N
      7N  2N 1N
      100N 7N 2N
      15N 4N 3N))

  (testing "modulo with decimals"
    (let [r (ev/convert-request {:op :modulo :dividend 10.5M :divisor 3M})]
      (is (:ok? r))
      (is (= 1.5M (:value r))))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-modulo-math-expression
  (testing "% operator in math expression"
    (let [{:keys [result]} (cli/process-request-text "10 % 4" nil)]
      (is (= "2" result)))
    (let [{:keys [result]} (cli/process-request-text "10%3" nil)]
      (is (= "1" result)))
    (let [{:keys [result]} (cli/process-request-text "100 % 7" nil)]
      (is (= "2" result)))))

(deftest end-to-end-modulo-natural-language
  (testing "mod keyword"
    (let [{:keys [result]} (cli/process-request-text "10 mod 4" nil)]
      (is (= "2" result)))
    (let [{:keys [result]} (cli/process-request-text "10 mod 3" nil)]
      (is (= "1" result))))

  (testing "modulo keyword"
    (let [{:keys [result]} (cli/process-request-text "15 modulo 4" nil)]
      (is (= "3" result))))

  (testing "what is X mod Y"
    (let [{:keys [result]} (cli/process-request-text "what is 10 mod 3" nil)]
      (is (= "1" result)))))
