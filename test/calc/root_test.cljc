(ns calc.root-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.test-helpers :as th]))

;; ==========================================================================
;; Parser tests — math expression syntax: sqrt(), cbrt(), root()
;; ==========================================================================

(deftest parses-sqrt-in-math-expressions
  (testing "sqrt() in parenthesised math context"
    (are [expr expected] (th/deep== expected (parser/parse-math expr))
      "sqrt(144)"   12
      "sqrt(25)"    5
      "sqrt(16)"    4
      "sqrt(1)"     1
      "sqrt(0)"     0
      "sqrt(10000)" 100))

  (testing "sqrt() with imperfect squares returns decimal"
    (let [r (parser/parse-math "sqrt(2)")]
      (is (some? r))
      (is (> r 1.414))
      (is (< r 1.415))))

  (testing "sqrt() composes with arithmetic"
    (are [expr expected] (th/deep== expected (parser/parse-math expr))
      "sqrt(9) + 1"     4
      "sqrt(9) * 2"     6
      "2 * sqrt(16)"    8
      "sqrt(4) + sqrt(9)" 5
      "sqrt(4) ^ 2"     4
      "sqrt(100) - 5"   5))

  (testing "sqrt() in unit conversion quantity"
    (is (th/deep== {:op :convert
            :quantity {:value 12 :unit :ft}
            :to :m}
           (parser/parse-request "sqrt(144) feet in meters")))))

(deftest parses-cbrt-in-math-expressions
  (testing "cbrt() perfect cubes"
    (are [expr expected] (th/deep== expected (parser/parse-math expr))
      "cbrt(27)"    3
      "cbrt(8)"     2
      "cbrt(64)"    4
      "cbrt(125)"   5
      "cbrt(1000)"  10
      "cbrt(1)"     1))

  (testing "cbrt() imperfect cubes"
    (let [r (parser/parse-math "cbrt(2)")]
      (is (some? r))
      (is (> r 1.259))
      (is (< r 1.260))))

  (testing "cbrt() composes with arithmetic"
    (are [expr expected] (th/deep== expected (parser/parse-math expr))
      "cbrt(27) + 1"   4
      "cbrt(8) * 3"    6
      "2 + cbrt(125)"  7)))

(deftest parses-root-function-in-math-expressions
  (testing "root(n, x) for nth roots"
    (are [expr expected] (th/deep== expected (parser/parse-math expr))
      "root(2, 144)"   12
      "root(3, 27)"    3
      "root(4, 16)"    2
      "root(4, 81)"    3
      "root(5, 32)"    2
      "root(4, 625)"   5))

  (testing "root(n, x) with imperfect roots"
    (let [r (parser/parse-math "root(4, 2)")]
      (is (some? r))
      (is (> r 1.189))
      (is (< r 1.190))))

  (testing "root() composes with arithmetic"
    (is (th/deep== 7 (parser/parse-math "root(3, 125) + 2")))))

(deftest nested-root-expressions
  (testing "sqrt of sqrt"
    (is (th/deep== 3 (parser/parse-math "sqrt(sqrt(81))"))))

  (testing "cbrt inside arithmetic"
    (is (th/deep== 10 (parser/parse-math "cbrt(27) + cbrt(343)")))))

;; ==========================================================================
;; Parser tests — natural language root forms
;; ==========================================================================

(deftest parses-square-root-natural-language
  (testing "'square root of X' form"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "square root of 144"
      {:op :root :degree 2 :value 144}

      "square root of 25"
      {:op :root :degree 2 :value 25}

      "square root of 2"
      {:op :root :degree 2 :value 2}))

  (testing "'what is the square root of X' form"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "what is the square root of 144"
      {:op :root :degree 2 :value 144}

      "what is the square root of 49"
      {:op :root :degree 2 :value 49}))

  (testing "'sqrt X' and 'sqrt of X' shorthand"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "sqrt 144"
      {:op :root :degree 2 :value 144}

      "sqrt of 144"
      {:op :root :degree 2 :value 144}

      "sqrt 2"
      {:op :root :degree 2 :value 2})))

(deftest parses-cube-root-natural-language
  (testing "'cube root of X' form"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "cube root of 27"
      {:op :root :degree 3 :value 27}

      "cube root of 125"
      {:op :root :degree 3 :value 125}))

  (testing "'what is the cube root of X' form"
    (is (th/deep== {:op :root :degree 3 :value 64}
           (parser/parse-request "what is the cube root of 64"))))

  (testing "'cbrt X' and 'cbrt of X' shorthand"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "cbrt 27"
      {:op :root :degree 3 :value 27}

      "cbrt of 125"
      {:op :root :degree 3 :value 125})))

(deftest parses-nth-root-natural-language
  (testing "ordinal forms: '4th root of X', '5th root of X'"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "4th root of 16"
      {:op :root :degree 4 :value 16}

      "5th root of 32"
      {:op :root :degree 5 :value 32}

      "6th root of 64"
      {:op :root :degree 6 :value 64}))

  (testing "word ordinal forms: 'fourth root of X', 'fifth root of X'"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "fourth root of 81"
      {:op :root :degree 4 :value 81}

      "fifth root of 32"
      {:op :root :degree 5 :value 32}

      "third root of 27"
      {:op :root :degree 3 :value 27}))

  (testing "'what is the Nth root of X' forms"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "what is the 4th root of 625"
      {:op :root :degree 4 :value 625}

      "what is the fifth root of 32"
      {:op :root :degree 5 :value 32})))

(deftest parses-root-with-formatting
  (testing "root with format suffix"
    (is (th/deep== {:op :root :degree 2 :value 2 :format {:round 4}}
           (parser/parse-request "square root of 2 rounded to 4 decimals")))
    (is (th/deep== {:op :root :degree 3 :value 2 :format {:sig-figs 5}}
           (parser/parse-request "cube root of 2 with 5 sig figs")))))

(deftest parses-root-with-decimal-and-fraction-inputs
  (testing "decimal input"
    (is (th/deep== {:op :root :degree 2 :value 2.25}
           (parser/parse-request "square root of 2.25"))))

  (testing "fraction input"
    (is (th/deep== {:op :root :degree 2 :value (/ 1 4)}
           (parser/parse-request "square root of 1/4")))))

;; ==========================================================================
;; Eval tests — root evaluation with perfect root detection
;; ==========================================================================

(deftest evaluates-perfect-square-roots
  (testing "perfect squares return exact integers"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 2 :value n})]
                        (and (:ok? r) (th/approx== expected (:value r))))
      0    0
      1    1
      4    2
      9    3
      16   4
      25   5
      36   6
      49   7
      64   8
      81   9
      100  10
      144  12
      169  13
      256  16
      10000 100)))

(deftest evaluates-imperfect-square-roots
  (testing "non-perfect squares return decimals"
    (let [r (ev/convert-request {:op :root :degree 2 :value 2})]
      (is (:ok? r))
      (is (> (:value r) 1.414))
      (is (< (:value r) 1.415)))
    (let [r (ev/convert-request {:op :root :degree 2 :value 3})]
      (is (:ok? r))
      (is (> (:value r) 1.732))
      (is (< (:value r) 1.733)))))

(deftest evaluates-perfect-cube-roots
  (testing "perfect cubes return exact integers"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 3 :value n})]
                        (and (:ok? r) (th/approx== expected (:value r))))
      1    1
      8    2
      27   3
      64   4
      125  5
      216  6
      343  7
      512  8
      729  9
      1000 10)))

(deftest evaluates-perfect-nth-roots
  (testing "perfect 4th roots"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 4 :value n})]
                        (and (:ok? r) (th/approx== expected (:value r))))
      16   2
      81   3
      256  4
      625  5))

  (testing "perfect 5th roots"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 5 :value n})]
                        (and (:ok? r) (th/approx== expected (:value r))))
      32   2
      243  3
      1024 4
      3125 5))

  (testing "perfect 6th root"
    (let [r (ev/convert-request {:op :root :degree 6 :value 64})]
      (is (:ok? r))
      (is (th/approx== 2 (:value r)))))
)

(deftest evaluates-imperfect-nth-roots
  (testing "imperfect 4th root"
    (let [r (ev/convert-request {:op :root :degree 4 :value 2})]
      (is (:ok? r))
      (is (> (:value r) 1.189))
      (is (< (:value r) 1.190))))

  (testing "imperfect 5th root"
    (let [r (ev/convert-request {:op :root :degree 5 :value 10})]
      (is (:ok? r))
      (is (> (:value r) 1.584))
      (is (< (:value r) 1.586)))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-sqrt-expressions
  (testing "sqrt in math expression"
    (let [{:keys [result]} (th/evaluate "sqrt(144)" nil)]
      (is (= "12" result))))

  (testing "sqrt of imperfect square via math"
    (let [{:keys [result]} (th/evaluate "sqrt(2)" nil)]
      (is (some? result))
      (is (str/starts-with? result "1.414"))))

  (testing "sqrt in unit conversion"
    (let [{:keys [result target]} (th/evaluate "sqrt(144) feet in meters" nil)]
      (is (some? result))
      (is (= "m" target)))))

(deftest end-to-end-natural-language-roots
  (testing "square root of perfect square"
    (let [{:keys [result]} (th/evaluate "square root of 144" nil)]
      (is (= "12" result))))

  (testing "square root of imperfect square"
    (let [{:keys [result]} (th/evaluate "square root of 2" nil)]
      (is (some? result))
      (is (str/starts-with? result "1.414"))))

  (testing "cube root"
    (let [{:keys [result]} (th/evaluate "cube root of 27" nil)]
      (is (= "3" result))))

  (testing "what is the square root of"
    (let [{:keys [result]} (th/evaluate "what is the square root of 49" nil)]
      (is (= "7" result))))

  (testing "what is the cube root of"
    (let [{:keys [result]} (th/evaluate "what is the cube root of 64" nil)]
      (is (= "4" result))))

  (testing "4th root"
    (let [{:keys [result]} (th/evaluate "4th root of 625" nil)]
      (is (= "5" result))))

  (testing "fifth root"
    (let [{:keys [result]} (th/evaluate "fifth root of 32" nil)]
      (is (= "2" result))))

  (testing "sqrt shorthand"
    (let [{:keys [result]} (th/evaluate "sqrt 144" nil)]
      (is (= "12" result))))

  (testing "cbrt shorthand"
    (let [{:keys [result]} (th/evaluate "cbrt 27" nil)]
      (is (= "3" result))))

  (testing "cbrt of shorthand"
    (let [{:keys [result]} (th/evaluate "cbrt of 125" nil)]
      (is (= "5" result)))))

(deftest end-to-end-root-with-formatting
  (testing "square root with rounding"
    (let [{:keys [result]} (th/evaluate "square root of 2 rounded to 4 decimals" nil)]
      (is (= "1.4142" result))))

  (testing "cube root with sig figs"
    (let [{:keys [result]} (th/evaluate "cube root of 2 with 5 sig figs" nil)]
      (is (= "1.2599" result)))))

(deftest end-to-end-root-large-numbers
  (testing "large perfect square"
    (let [{:keys [result]} (th/evaluate "square root of 1000000" nil)]
      (is (= "1000" result))))

  (testing "large perfect cube"
    (let [{:keys [result]} (th/evaluate "cube root of 1000000" nil)]
      (is (= "100" result)))))

(deftest end-to-end-nested-sqrt-math
  (testing "sqrt composed with math"
    (let [{:keys [result]} (th/evaluate "sqrt(9) + sqrt(16)" nil)]
      (is (= "7" result))))

  (testing "sqrt times a number"
    (let [{:keys [result]} (th/evaluate "2 * sqrt(25)" nil)]
      (is (= "10" result))))

  (testing "cbrt in arithmetic"
    (let [{:keys [result]} (th/evaluate "cbrt(8) + cbrt(27)" nil)]
      (is (= "5" result)))))

(deftest end-to-end-root-function-syntax
  (testing "root(n, x) syntax"
    (let [{:keys [result]} (th/evaluate "root(3, 125)" nil)]
      (is (= "5" result))))

  (testing "root(4, 256)"
    (let [{:keys [result]} (th/evaluate "root(4, 256)" nil)]
      (is (= "4" result))))

  (testing "root(2, 144) same as sqrt"
    (let [{:keys [result]} (th/evaluate "root(2, 144)" nil)]
      (is (= "12" result)))))

(deftest end-to-end-decimal-input-roots
  (testing "square root of decimal"
    (let [{:keys [result]} (th/evaluate "square root of 2.25" nil)]
      (is (= "1.5" result))))

  (testing "sqrt of decimal via math"
    (let [{:keys [result]} (th/evaluate "sqrt(6.25)" nil)]
      (is (= "2.5" result)))))
