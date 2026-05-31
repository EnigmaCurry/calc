(ns calc.tip-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.cli :as cli]))

;; ==========================================================================
;; Parser tests — natural language tip forms
;; ==========================================================================

(deftest parses-tip-percent-on-bill
  (testing "'X percent tip on Y' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "20 percent tip on 50"
      {:op :tip :percent 20N :bill 50N}

      "15 percent tip on 85.50"
      {:op :tip :percent 15N :bill 85.50M}

      "18 percent tip on 100"
      {:op :tip :percent 18N :bill 100N})))

(deftest parses-tip-with-dollar-signs
  (testing "dollar signs are stripped"
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "20 percent tip on $50")))
    (is (= {:op :tip :percent 15N :bill 85.50M}
           (parser/parse-request "15 percent tip on $85.50")))))

(deftest parses-tip-on-at-form
  (testing "'tip on Y at X%' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "tip on 50 at 20 percent"
      {:op :tip :percent 20N :bill 50N}

      "tip on $85.50 at 18 percent"
      {:op :tip :percent 18N :bill 85.50M}))

  (testing "'what is the tip on Y at X%' form"
    (is (= {:op :tip :percent 18N :bill 85.50M}
           (parser/parse-request "what is the tip on $85.50 at 18 percent")))))

(deftest parses-tip-x-percent-on-form
  (testing "'tip X% on Y' form"
    (is (= {:op :tip :percent 20N :bill 100N}
           (parser/parse-request "tip 20 percent on 100")))
    (is (= {:op :tip :percent 15N :bill 50N}
           (parser/parse-request "tip 15 percent on $50")))))

(deftest parses-tip-for-form
  (testing "'tip for Y' works like 'tip on Y'"
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "tip for 50")))
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "tip for $50"))))

  (testing "'tip for Y at X%' works like 'tip on Y at X%'"
    (is (= {:op :tip :percent 15N :bill 50N}
           (parser/parse-request "tip for $50 at 15 percent"))))

  (testing "'X percent tip for Y' works like 'X percent tip on Y'"
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "20 percent tip for 50"))))

  (testing "'tip X percent for Y' works like 'tip X percent on Y'"
    (is (= {:op :tip :percent 15N :bill 50N}
           (parser/parse-request "tip 15 percent for $50")))))

(deftest parses-tip-default-percent
  (testing "'tip on Y' defaults to 20%"
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "tip on 50")))
    (is (= {:op :tip :percent 20N :bill 85.50M}
           (parser/parse-request "tip on $85.50")))))

;; ==========================================================================
;; Parser tests — brief/shorthand tip forms
;; ==========================================================================

(deftest parses-tip-brief-percent-bill
  (testing "'tip X% $Y' — percent then bill"
    (is (= {:op :tip :percent 10N :bill 50N}
           (parser/parse-request "tip 10% $50")))
    (is (= {:op :tip :percent 15N :bill 85.50M}
           (parser/parse-request "tip 15% $85.50")))))

(deftest parses-tip-brief-bill-percent
  (testing "'tip $Y X%' — bill then percent"
    (is (= {:op :tip :percent 10N :bill 50N}
           (parser/parse-request "tip $50 10%")))
    (is (= {:op :tip :percent 18N :bill 100N}
           (parser/parse-request "tip $100 18%")))))

(deftest parses-tip-brief-dollar-only
  (testing "'tip $Y' — dollar amount defaults to 20%"
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "tip $50")))
    (is (= {:op :tip :percent 20N :bill 85.50M}
           (parser/parse-request "tip $85.50")))))

(deftest parses-tip-brief-bare-number
  (testing "'tip N' — bare number defaults to $N at 20%"
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "tip 50")))
    (is (= {:op :tip :percent 20N :bill 100N}
           (parser/parse-request "tip 100")))))

(deftest parses-tip-brief-two-bare-numbers
  (testing "'tip N M' — first is bill, second is percent"
    (is (= {:op :tip :percent 25N :bill 50N}
           (parser/parse-request "tip 50 25")))
    (is (= {:op :tip :percent 18N :bill 100N}
           (parser/parse-request "tip 100 18")))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-tip
  (testing "basic tip calculations"
    (are [percent bill exp-tip exp-total]
      (let [r (ev/convert-request {:op :tip :percent percent :bill bill})]
        (and (:ok? r) (= exp-tip (:tip r)) (= exp-total (:total r))))
      20N 50N    10N  60N
      15N 100N   15N  115N
      18N 50N    9N   59N
      20N 85.50M 17.1M 102.6M
      25N 40N    10N  50N))

  (testing "tip with zero percent"
    (let [r (ev/convert-request {:op :tip :percent 0N :bill 50N})]
      (is (:ok? r))
      (is (= 0N (:tip r)))
      (is (= 50N (:total r))))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-tip
  (testing "X% tip on Y"
    (let [{:keys [result]} (cli/process-request-text "20 percent tip on 50" nil)]
      (is (= "Bill: $50, Tip: $10 (20%), Total: $60" result))))

  (testing "tip on Y at X%"
    (let [{:keys [result]} (cli/process-request-text "tip on $100 at 15 percent" nil)]
      (is (= "Bill: $100, Tip: $15 (15%), Total: $115" result))))

  (testing "tip on Y (default 20%)"
    (let [{:keys [result]} (cli/process-request-text "tip on $50" nil)]
      (is (= "Bill: $50, Tip: $10 (20%), Total: $60" result))))

  (testing "tip with decimals"
    (let [{:keys [result]} (cli/process-request-text "18 percent tip on $85.50" nil)]
      (is (= "Bill: $85.5, Tip: $15.39 (18%), Total: $100.89" result)))))
