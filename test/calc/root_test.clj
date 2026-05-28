(ns calc.root-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.cli :as cli]))

;; ==========================================================================
;; Parser tests — math expression syntax: sqrt(), cbrt(), root()
;; ==========================================================================

(deftest parses-sqrt-in-math-expressions
  (testing "sqrt() in parenthesised math context"
    (are [expr expected] (= expected (parser/parse-math expr))
      "sqrt(144)"   12N
      "sqrt(25)"    5N
      "sqrt(16)"    4N
      "sqrt(1)"     1N
      "sqrt(0)"     0N
      "sqrt(10000)" 100N))

  (testing "sqrt() with imperfect squares returns decimal"
    (let [r (parser/parse-math "sqrt(2)")]
      (is (some? r))
      (is (> r 1.414))
      (is (< r 1.415))))

  (testing "sqrt() composes with arithmetic"
    (are [expr expected] (= expected (parser/parse-math expr))
      "sqrt(9) + 1"     4N
      "sqrt(9) * 2"     6N
      "2 * sqrt(16)"    8N
      "sqrt(4) + sqrt(9)" 5N
      "sqrt(4) ^ 2"     4N
      "sqrt(100) - 5"   5N))

  (testing "sqrt() in unit conversion quantity"
    (is (= {:op :convert
            :quantity {:value 12N :unit :ft}
            :to :m}
           (parser/parse-request "sqrt(144) feet in meters")))))

(deftest parses-cbrt-in-math-expressions
  (testing "cbrt() perfect cubes"
    (are [expr expected] (= expected (parser/parse-math expr))
      "cbrt(27)"    3N
      "cbrt(8)"     2N
      "cbrt(64)"    4N
      "cbrt(125)"   5N
      "cbrt(1000)"  10N
      "cbrt(1)"     1N))

  (testing "cbrt() imperfect cubes"
    (let [r (parser/parse-math "cbrt(2)")]
      (is (some? r))
      (is (> r 1.259))
      (is (< r 1.260))))

  (testing "cbrt() composes with arithmetic"
    (are [expr expected] (= expected (parser/parse-math expr))
      "cbrt(27) + 1"   4N
      "cbrt(8) * 3"    6N
      "2 + cbrt(125)"  7N)))

(deftest parses-root-function-in-math-expressions
  (testing "root(n, x) for nth roots"
    (are [expr expected] (= expected (parser/parse-math expr))
      "root(2, 144)"   12N
      "root(3, 27)"    3N
      "root(4, 16)"    2N
      "root(4, 81)"    3N
      "root(5, 32)"    2N
      "root(4, 625)"   5N))

  (testing "root(n, x) with imperfect roots"
    (let [r (parser/parse-math "root(4, 2)")]
      (is (some? r))
      (is (> r 1.189))
      (is (< r 1.190))))

  (testing "root() composes with arithmetic"
    (is (= 7N (parser/parse-math "root(3, 125) + 2")))))

(deftest nested-root-expressions
  (testing "sqrt of sqrt"
    (is (= 3N (parser/parse-math "sqrt(sqrt(81))"))))

  (testing "cbrt inside arithmetic"
    (is (= 10N (parser/parse-math "cbrt(27) + cbrt(343)")))))

;; ==========================================================================
;; Parser tests — natural language root forms
;; ==========================================================================

(deftest parses-square-root-natural-language
  (testing "'square root of X' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "square root of 144"
      {:op :root :degree 2 :value 144N}

      "square root of 25"
      {:op :root :degree 2 :value 25N}

      "square root of 2"
      {:op :root :degree 2 :value 2N}))

  (testing "'what is the square root of X' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "what is the square root of 144"
      {:op :root :degree 2 :value 144N}

      "what is the square root of 49"
      {:op :root :degree 2 :value 49N}))

  (testing "'sqrt X' and 'sqrt of X' shorthand"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "sqrt 144"
      {:op :root :degree 2 :value 144N}

      "sqrt of 144"
      {:op :root :degree 2 :value 144N}

      "sqrt 2"
      {:op :root :degree 2 :value 2N})))

(deftest parses-cube-root-natural-language
  (testing "'cube root of X' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "cube root of 27"
      {:op :root :degree 3 :value 27N}

      "cube root of 125"
      {:op :root :degree 3 :value 125N}))

  (testing "'what is the cube root of X' form"
    (is (= {:op :root :degree 3 :value 64N}
           (parser/parse-request "what is the cube root of 64"))))

  (testing "'cbrt X' and 'cbrt of X' shorthand"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "cbrt 27"
      {:op :root :degree 3 :value 27N}

      "cbrt of 125"
      {:op :root :degree 3 :value 125N})))

(deftest parses-nth-root-natural-language
  (testing "ordinal forms: '4th root of X', '5th root of X'"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "4th root of 16"
      {:op :root :degree 4 :value 16N}

      "5th root of 32"
      {:op :root :degree 5 :value 32N}

      "6th root of 64"
      {:op :root :degree 6 :value 64N}))

  (testing "word ordinal forms: 'fourth root of X', 'fifth root of X'"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "fourth root of 81"
      {:op :root :degree 4 :value 81N}

      "fifth root of 32"
      {:op :root :degree 5 :value 32N}

      "third root of 27"
      {:op :root :degree 3 :value 27N}))

  (testing "'what is the Nth root of X' forms"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "what is the 4th root of 625"
      {:op :root :degree 4 :value 625N}

      "what is the fifth root of 32"
      {:op :root :degree 5 :value 32N})))

(deftest parses-root-with-formatting
  (testing "root with format suffix"
    (is (= {:op :root :degree 2 :value 2N :format {:round 4}}
           (parser/parse-request "square root of 2 rounded to 4 decimals")))
    (is (= {:op :root :degree 3 :value 2N :format {:sig-figs 5}}
           (parser/parse-request "cube root of 2 with 5 sig figs")))))

(deftest parses-root-with-decimal-and-fraction-inputs
  (testing "decimal input"
    (is (= {:op :root :degree 2 :value 2.25M}
           (parser/parse-request "square root of 2.25"))))

  (testing "fraction input"
    (is (= {:op :root :degree 2 :value 1/4}
           (parser/parse-request "square root of 1/4")))))

;; ==========================================================================
;; Eval tests — root evaluation with perfect root detection
;; ==========================================================================

(deftest evaluates-perfect-square-roots
  (testing "perfect squares return exact integers"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 2 :value n})]
                        (and (:ok? r) (= expected (:value r))))
      0N    0N
      1N    1N
      4N    2N
      9N    3N
      16N   4N
      25N   5N
      36N   6N
      49N   7N
      64N   8N
      81N   9N
      100N  10N
      144N  12N
      169N  13N
      256N  16N
      10000N 100N)))

(deftest evaluates-imperfect-square-roots
  (testing "non-perfect squares return decimals"
    (let [r (ev/convert-request {:op :root :degree 2 :value 2N})]
      (is (:ok? r))
      (is (> (:value r) 1.414))
      (is (< (:value r) 1.415)))
    (let [r (ev/convert-request {:op :root :degree 2 :value 3N})]
      (is (:ok? r))
      (is (> (:value r) 1.732))
      (is (< (:value r) 1.733)))))

(deftest evaluates-perfect-cube-roots
  (testing "perfect cubes return exact integers"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 3 :value n})]
                        (and (:ok? r) (= expected (:value r))))
      1N    1N
      8N    2N
      27N   3N
      64N   4N
      125N  5N
      216N  6N
      343N  7N
      512N  8N
      729N  9N
      1000N 10N)))

(deftest evaluates-perfect-nth-roots
  (testing "perfect 4th roots"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 4 :value n})]
                        (and (:ok? r) (= expected (:value r))))
      16N   2N
      81N   3N
      256N  4N
      625N  5N))

  (testing "perfect 5th roots"
    (are [n expected] (let [r (ev/convert-request {:op :root :degree 5 :value n})]
                        (and (:ok? r) (= expected (:value r))))
      32N   2N
      243N  3N
      1024N 4N
      3125N 5N))

  (testing "perfect 6th root"
    (let [r (ev/convert-request {:op :root :degree 6 :value 64N})]
      (is (:ok? r))
      (is (= 2N (:value r))))))

(deftest evaluates-imperfect-nth-roots
  (testing "imperfect 4th root"
    (let [r (ev/convert-request {:op :root :degree 4 :value 2N})]
      (is (:ok? r))
      (is (> (:value r) 1.189))
      (is (< (:value r) 1.190))))

  (testing "imperfect 5th root"
    (let [r (ev/convert-request {:op :root :degree 5 :value 10N})]
      (is (:ok? r))
      (is (> (:value r) 1.584))
      (is (< (:value r) 1.586)))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-sqrt-expressions
  (testing "sqrt in math expression"
    (let [{:keys [result]} (cli/process-request-text "sqrt(144)" nil)]
      (is (= "12" result))))

  (testing "sqrt of imperfect square via math"
    (let [{:keys [result]} (cli/process-request-text "sqrt(2)" nil)]
      (is (some? result))
      (is (.startsWith result "1.414"))))

  (testing "sqrt in unit conversion"
    (let [{:keys [result target]} (cli/process-request-text "sqrt(144) feet in meters" nil)]
      (is (some? result))
      (is (= "m" target)))))

(deftest end-to-end-natural-language-roots
  (testing "square root of perfect square"
    (let [{:keys [result]} (cli/process-request-text "square root of 144" nil)]
      (is (= "12" result))))

  (testing "square root of imperfect square"
    (let [{:keys [result]} (cli/process-request-text "square root of 2" nil)]
      (is (some? result))
      (is (.startsWith result "1.414"))))

  (testing "cube root"
    (let [{:keys [result]} (cli/process-request-text "cube root of 27" nil)]
      (is (= "3" result))))

  (testing "what is the square root of"
    (let [{:keys [result]} (cli/process-request-text "what is the square root of 49" nil)]
      (is (= "7" result))))

  (testing "what is the cube root of"
    (let [{:keys [result]} (cli/process-request-text "what is the cube root of 64" nil)]
      (is (= "4" result))))

  (testing "4th root"
    (let [{:keys [result]} (cli/process-request-text "4th root of 625" nil)]
      (is (= "5" result))))

  (testing "fifth root"
    (let [{:keys [result]} (cli/process-request-text "fifth root of 32" nil)]
      (is (= "2" result))))

  (testing "sqrt shorthand"
    (let [{:keys [result]} (cli/process-request-text "sqrt 144" nil)]
      (is (= "12" result))))

  (testing "cbrt shorthand"
    (let [{:keys [result]} (cli/process-request-text "cbrt 27" nil)]
      (is (= "3" result))))

  (testing "cbrt of shorthand"
    (let [{:keys [result]} (cli/process-request-text "cbrt of 125" nil)]
      (is (= "5" result)))))

(deftest end-to-end-root-with-formatting
  (testing "square root with rounding"
    (let [{:keys [result]} (cli/process-request-text "square root of 2 rounded to 4 decimals" nil)]
      (is (= "1.4142" result))))

  (testing "cube root with sig figs"
    (let [{:keys [result]} (cli/process-request-text "cube root of 2 with 5 sig figs" nil)]
      (is (= "1.2599" result)))))

(deftest end-to-end-root-large-numbers
  (testing "large perfect square"
    (let [{:keys [result]} (cli/process-request-text "square root of 1000000" nil)]
      (is (= "1000" result))))

  (testing "large perfect cube"
    (let [{:keys [result]} (cli/process-request-text "cube root of 1000000" nil)]
      (is (= "100" result)))))

(deftest end-to-end-nested-sqrt-math
  (testing "sqrt composed with math"
    (let [{:keys [result]} (cli/process-request-text "sqrt(9) + sqrt(16)" nil)]
      (is (= "7" result))))

  (testing "sqrt times a number"
    (let [{:keys [result]} (cli/process-request-text "2 * sqrt(25)" nil)]
      (is (= "10" result))))

  (testing "cbrt in arithmetic"
    (let [{:keys [result]} (cli/process-request-text "cbrt(8) + cbrt(27)" nil)]
      (is (= "5" result)))))

(deftest end-to-end-root-function-syntax
  (testing "root(n, x) syntax"
    (let [{:keys [result]} (cli/process-request-text "root(3, 125)" nil)]
      (is (= "5" result))))

  (testing "root(4, 256)"
    (let [{:keys [result]} (cli/process-request-text "root(4, 256)" nil)]
      (is (= "4" result))))

  (testing "root(2, 144) same as sqrt"
    (let [{:keys [result]} (cli/process-request-text "root(2, 144)" nil)]
      (is (= "12" result)))))

(deftest end-to-end-decimal-input-roots
  (testing "square root of decimal"
    (let [{:keys [result]} (cli/process-request-text "square root of 2.25" nil)]
      (is (= "1.5" result))))

  (testing "sqrt of decimal via math"
    (let [{:keys [result]} (cli/process-request-text "sqrt(6.25)" nil)]
      (is (= "2.5" result)))))
