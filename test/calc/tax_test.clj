(ns calc.tax-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.cli :as cli]))

;; ==========================================================================
;; Parser tests — natural language tax forms
;; ==========================================================================

(deftest parses-tax-percent-on-price
  (testing "'X percent tax on Y' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "10 percent tax on 50"
      {:op :tax :percent 10N :price 50N}

      "8.25 percent tax on 100"
      {:op :tax :percent 8.25M :price 100N}

      "6 percent tax on 29.99"
      {:op :tax :percent 6N :price 29.99M})))

(deftest parses-tax-with-dollar-signs
  (testing "dollar signs are stripped"
    (is (= {:op :tax :percent 10N :price 50N}
           (parser/parse-request "10 percent tax on $50")))
    (is (= {:op :tax :percent 8.25M :price 99.99M}
           (parser/parse-request "8.25 percent tax on $99.99")))))

(deftest parses-tax-on-at-form
  (testing "'tax on Y at X%' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))
      "tax on 50 at 10 percent"
      {:op :tax :percent 10N :price 50N}

      "tax on $99.99 at 8.25 percent"
      {:op :tax :percent 8.25M :price 99.99M}))

  (testing "'what is the tax on Y at X%' form"
    (is (= {:op :tax :percent 8.25M :price 99.99M}
           (parser/parse-request "what is the tax on $99.99 at 8.25 percent")))))

(deftest parses-tax-x-percent-on-form
  (testing "'tax X% on Y' form"
    (is (= {:op :tax :percent 10N :price 100N}
           (parser/parse-request "tax 10 percent on 100")))
    (is (= {:op :tax :percent 6N :price 50N}
           (parser/parse-request "tax 6 percent on $50")))))

(deftest parses-tax-for-form
  (testing "'tax for Y at X%' works like 'tax on Y at X%'"
    (is (= {:op :tax :percent 10N :price 50N}
           (parser/parse-request "tax for $50 at 10 percent"))))

  (testing "'X percent tax for Y' works like 'X percent tax on Y'"
    (is (= {:op :tax :percent 10N :price 50N}
           (parser/parse-request "10 percent tax for 50"))))

  (testing "'tax X percent for Y' works like 'tax X percent on Y'"
    (is (= {:op :tax :percent 6N :price 50N}
           (parser/parse-request "tax 6 percent for $50")))))

;; ==========================================================================
;; Parser tests — brief/shorthand tax forms
;; ==========================================================================

(deftest parses-tax-brief-percent-price
  (testing "'tax X% $Y' — percent then price"
    (is (= {:op :tax :percent 10N :price 50N}
           (parser/parse-request "tax 10% $50")))
    (is (= {:op :tax :percent 8.25M :price 99.99M}
           (parser/parse-request "tax 8.25% $99.99")))))

(deftest parses-tax-brief-price-percent
  (testing "'tax $Y X%' — dollar price then percent"
    (is (= {:op :tax :percent 10N :price 50N}
           (parser/parse-request "tax $50 10%")))
    (is (= {:op :tax :percent 8.25M :price 100N}
           (parser/parse-request "tax $100 8.25%"))))

  (testing "'tax $Y N' — dollar price then bare number rate"
    (is (= {:op :tax :percent 10N :price 500N}
           (parser/parse-request "tax $500 10"))))

  (testing "'tax N X%' — bare price then percent"
    (is (= {:op :tax :percent 10N :price 500N}
           (parser/parse-request "tax 500 10%")))))

(deftest parses-tax-brief-two-bare-numbers
  (testing "'tax N M' — first is price, second is rate"
    (is (= {:op :tax :percent 10N :price 50N}
           (parser/parse-request "tax 50 10")))
    (is (= {:op :tax :percent 8N :price 100N}
           (parser/parse-request "tax 100 8")))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-tax
  (testing "basic tax calculations"
    (are [percent price exp-tax exp-total]
      (let [r (ev/convert-request {:op :tax :percent percent :price price})]
        (and (:ok? r) (= exp-tax (:tax r)) (= exp-total (:total r))))
      10N  50N    5N    55N
      10N  100N   10N   110N
      8N   50N    4N    54N
      6N   29.99M 1.7994M 31.7894M
      25N  40N    10N   50N))

  (testing "tax with zero percent"
    (let [r (ev/convert-request {:op :tax :percent 0N :price 50N})]
      (is (:ok? r))
      (is (= 0N (:tax r)))
      (is (= 50N (:total r))))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-tax
  (testing "X% tax on Y"
    (let [{:keys [result]} (cli/process-request-text "10 percent tax on 50" nil)]
      (is (= "Price: $50, Tax: $5 (10%), Total: $55" result))))

  (testing "tax on Y at X%"
    (let [{:keys [result]} (cli/process-request-text "tax on $100 at 10 percent" nil)]
      (is (= "Price: $100, Tax: $10 (10%), Total: $110" result))))

  (testing "brief: tax 50 10"
    (let [{:keys [result]} (cli/process-request-text "tax 50 10" nil)]
      (is (= "Price: $50, Tax: $5 (10%), Total: $55" result))))

  (testing "brief: tax $50 10%"
    (let [{:keys [result]} (cli/process-request-text "tax $50 10%" nil)]
      (is (= "Price: $50, Tax: $5 (10%), Total: $55" result))))

  (testing "tax with decimals"
    (let [{:keys [result]} (cli/process-request-text "8.25 percent tax on $99.99" nil)]
      (is (= "Price: $99.99, Tax: $8.249175 (8.25%), Total: $108.239175" result)))))
