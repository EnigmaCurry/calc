(ns calc.tax-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.test-helpers :as th]))

;; ==========================================================================
;; Parser tests — natural language tax forms
;; ==========================================================================

(deftest parses-tax-percent-on-price
  (testing "'X percent tax on Y' form"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "10 percent tax on 50"
      {:op :tax :percent 10 :price 50}

      "8.25 percent tax on 100"
      {:op :tax :percent 8.25 :price 100}

      "6 percent tax on 29.99"
      {:op :tax :percent 6 :price 29.99})))

(deftest parses-tax-with-dollar-signs
  (testing "dollar signs are stripped"
    (is (th/deep== {:op :tax :percent 10 :price 50}
           (parser/parse-request "10 percent tax on $50")))
    (is (th/deep== {:op :tax :percent 8.25 :price 99.99}
           (parser/parse-request "8.25 percent tax on $99.99")))))

(deftest parses-tax-on-at-form
  (testing "'tax on Y at X%' form"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "tax on 50 at 10 percent"
      {:op :tax :percent 10 :price 50}

      "tax on $99.99 at 8.25 percent"
      {:op :tax :percent 8.25 :price 99.99}))

  (testing "'what is the tax on Y at X%' form"
    (is (th/deep== {:op :tax :percent 8.25 :price 99.99}
           (parser/parse-request "what is the tax on $99.99 at 8.25 percent")))))

(deftest parses-tax-x-percent-on-form
  (testing "'tax X% on Y' form"
    (is (th/deep== {:op :tax :percent 10 :price 100}
           (parser/parse-request "tax 10 percent on 100")))
    (is (th/deep== {:op :tax :percent 6 :price 50}
           (parser/parse-request "tax 6 percent on $50")))))

(deftest parses-tax-for-form
  (testing "'tax for Y at X%' works like 'tax on Y at X%'"
    (is (th/deep== {:op :tax :percent 10 :price 50}
           (parser/parse-request "tax for $50 at 10 percent"))))

  (testing "'X percent tax for Y' works like 'X percent tax on Y'"
    (is (th/deep== {:op :tax :percent 10 :price 50}
           (parser/parse-request "10 percent tax for 50"))))

  (testing "'tax X percent for Y' works like 'tax X percent on Y'"
    (is (th/deep== {:op :tax :percent 6 :price 50}
           (parser/parse-request "tax 6 percent for $50")))))

;; ==========================================================================
;; Parser tests — brief/shorthand tax forms
;; ==========================================================================

(deftest parses-tax-brief-percent-price
  (testing "'tax X% $Y' — percent then price"
    (is (th/deep== {:op :tax :percent 10 :price 50}
           (parser/parse-request "tax 10% $50")))
    (is (th/deep== {:op :tax :percent 8.25 :price 99.99}
           (parser/parse-request "tax 8.25% $99.99")))))

(deftest parses-tax-brief-price-percent
  (testing "'tax $Y X%' — dollar price then percent"
    (is (th/deep== {:op :tax :percent 10 :price 50}
           (parser/parse-request "tax $50 10%")))
    (is (th/deep== {:op :tax :percent 8.25 :price 100}
           (parser/parse-request "tax $100 8.25%"))))

  (testing "'tax $Y N' — dollar price then bare number rate"
    (is (th/deep== {:op :tax :percent 10 :price 500}
           (parser/parse-request "tax $500 10"))))

  (testing "'tax N X%' — bare price then percent"
    (is (th/deep== {:op :tax :percent 10 :price 500}
           (parser/parse-request "tax 500 10%")))))

(deftest parses-tax-brief-two-bare-numbers
  (testing "'tax N M' — first is price, second is rate"
    (is (th/deep== {:op :tax :percent 10 :price 50}
           (parser/parse-request "tax 50 10")))
    (is (th/deep== {:op :tax :percent 8 :price 100}
           (parser/parse-request "tax 100 8")))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-tax
  (testing "basic tax calculations"
    (are [percent price exp-tax exp-total]
      (let [r (ev/convert-request {:op :tax :percent percent :price price})]
        (and (:ok? r) (== exp-tax (:tax r)) (== exp-total (:total r))))
      10  50    5    55
      10  100   10   110
      8   50    4    54
      6   29.99 1.80   31.79
      25  40    10   50))

  (testing "tax rounds up to the penny"
    (let [r (ev/convert-request {:op :tax :percent 0.2 :price 29.99})]
      (is (:ok? r))
      (is (== 0.06 (:tax r)))       ;; exact: 0.05998, rounds up to 0.06
      (is (== 30.05 (:total r))))
    (let [r (ev/convert-request {:op :tax :percent 8.25 :price 99.99})]
      (is (:ok? r))
      (is (== 8.25 (:tax r)))       ;; exact: 8.249175, rounds up to 8.25
      (is (== 108.24 (:total r)))))

  (testing "tax with zero percent"
    (let [r (ev/convert-request {:op :tax :percent 0 :price 50})]
      (is (:ok? r))
      (is (== 0 (:tax r)))
      (is (== 50 (:total r))))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-tax
  (testing "X% tax on Y"
    (let [{:keys [result]} (th/evaluate "10 percent tax on 50" nil)]
      (is (= "Price: $50.00, Tax: $5.00 (10%), Total: $55.00" result))))

  (testing "tax on Y at X%"
    (let [{:keys [result]} (th/evaluate "tax on $100 at 10 percent" nil)]
      (is (= "Price: $100.00, Tax: $10.00 (10%), Total: $110.00" result))))

  (testing "brief: tax 50 10"
    (let [{:keys [result]} (th/evaluate "tax 50 10" nil)]
      (is (= "Price: $50.00, Tax: $5.00 (10%), Total: $55.00" result))))

  (testing "brief: tax $50 10%"
    (let [{:keys [result]} (th/evaluate "tax $50 10%" nil)]
      (is (= "Price: $50.00, Tax: $5.00 (10%), Total: $55.00" result))))

  (testing "tax with decimals"
    (let [{:keys [result]} (th/evaluate "8.25 percent tax on $99.99" nil)]
      (is (= "Price: $99.99, Tax: $8.25 (8.25%), Total: $108.24" result)))))
