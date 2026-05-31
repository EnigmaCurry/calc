(ns calc.tip-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.cli :as cli]))

;; ==========================================================================
;; Parser tests — natural language tip forms (explicit percent)
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
  (testing "'tip for Y at X%' works like 'tip on Y at X%'"
    (is (= {:op :tip :percent 15N :bill 50N}
           (parser/parse-request "tip for $50 at 15 percent"))))

  (testing "'X percent tip for Y' works like 'X percent tip on Y'"
    (is (= {:op :tip :percent 20N :bill 50N}
           (parser/parse-request "20 percent tip for 50"))))

  (testing "'tip X percent for Y' works like 'tip X percent on Y'"
    (is (= {:op :tip :percent 15N :bill 50N}
           (parser/parse-request "tip 15 percent for $50")))))

;; ==========================================================================
;; Parser tests — round-tip forms (no explicit percent)
;; ==========================================================================

(deftest parses-tip-round-tip
  (testing "'tip on Y' triggers round-tip"
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip on 50")))
    (is (= {:op :tip :bill 85.50M :round-tip true}
           (parser/parse-request "tip on $85.50"))))

  (testing "'tip for Y' triggers round-tip"
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip for 50")))
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip for $50"))))

  (testing "'tip $Y' triggers round-tip"
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip $50")))
    (is (= {:op :tip :bill 85.50M :round-tip true}
           (parser/parse-request "tip $85.50"))))

  (testing "'tip N' triggers round-tip"
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip 50")))
    (is (= {:op :tip :bill 100N :round-tip true}
           (parser/parse-request "tip 100")))))

;; ==========================================================================
;; Parser tests — brief/shorthand tip forms (explicit percent)
;; ==========================================================================

(deftest parses-tip-brief-percent-bill
  (testing "'tip X% $Y' — percent then bill"
    (is (= {:op :tip :percent 10N :bill 50N}
           (parser/parse-request "tip 10% $50")))
    (is (= {:op :tip :percent 15N :bill 85.50M}
           (parser/parse-request "tip 15% $85.50")))))

(deftest parses-tip-brief-bill-percent
  (testing "'tip $Y X%' — dollar bill then percent"
    (is (= {:op :tip :percent 10N :bill 50N}
           (parser/parse-request "tip $50 10%")))
    (is (= {:op :tip :percent 18N :bill 100N}
           (parser/parse-request "tip $100 18%"))))

  (testing "'tip $Y N' — dollar bill then bare number percent"
    (is (= {:op :tip :percent 30N :bill 500N}
           (parser/parse-request "tip $500 30"))))

  (testing "'tip N X%' — bare bill then percent"
    (is (= {:op :tip :percent 30N :bill 500N}
           (parser/parse-request "tip 500 30%")))))

(deftest parses-tip-brief-two-bare-numbers
  (testing "'tip N M' — first is bill, second is percent"
    (is (= {:op :tip :percent 25N :bill 50N}
           (parser/parse-request "tip 50 25")))
    (is (= {:op :tip :percent 18N :bill 100N}
           (parser/parse-request "tip 100 18")))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-tip-explicit-percent
  (testing "basic tip calculations"
    (are [percent bill exp-tip exp-total]
      (let [r (ev/convert-request {:op :tip :percent percent :bill bill})]
        (and (:ok? r) (= exp-tip (:tip r)) (= exp-total (:total r))))
      20N 50N    10N  60N
      15N 100N   15N  115N
      18N 50N    9N   59N
      20N 85.50M 17.1M 102.6M
      25N 40N    10N  50N))

  (testing "tip rounds up to the penny"
    (let [r (ev/convert-request {:op :tip :percent 18N :bill 29.99M})]
      (is (:ok? r))
      (is (= 5.40M (:tip r)))
      (is (= 35.39M (:total r))))
    (let [r (ev/convert-request {:op :tip :percent 15N :bill 33.33M})]
      (is (:ok? r))
      (is (= 5N (:tip r)))
      (is (= 38.33M (:total r)))))

  (testing "tip with zero percent"
    (let [r (ev/convert-request {:op :tip :percent 0N :bill 50N})]
      (is (:ok? r))
      (is (= 0N (:tip r)))
      (is (= 50N (:total r))))))

(deftest evaluates-round-tip
  (testing "round tip picks cash-friendly amounts between 20-30%"
    ;; $50: 20%=$10, 30%=$15 → $10 (multiple of $10)
    (let [r (ev/convert-request {:op :tip :bill 50N :round-tip true})]
      (is (:ok? r))
      (is (= 10N (:tip r)))
      (is (= 60N (:total r))))

    ;; $85: 20%=$17, 30%=$25.50 → $20 (multiple of $20)
    (let [r (ev/convert-request {:op :tip :bill 85N :round-tip true})]
      (is (:ok? r))
      (is (= 20N (:tip r)))
      (is (= 105N (:total r))))

    ;; $47: 20%=$9.40, 30%=$14.10 → $10 (multiple of $10)
    (let [r (ev/convert-request {:op :tip :bill 47N :round-tip true})]
      (is (:ok? r))
      (is (= 10N (:tip r)))
      (is (= 57N (:total r))))

    ;; $12: 20%=$2.40, 30%=$3.60 → $3 (no $20/$10/$5 fits, $1 multiple)
    (let [r (ev/convert-request {:op :tip :bill 12N :round-tip true})]
      (is (:ok? r))
      (is (= 3N (:tip r)))
      (is (= 15N (:total r))))

    ;; $75: 20%=$15, 30%=$22.50 → $20 (multiple of $20)
    (let [r (ev/convert-request {:op :tip :bill 75N :round-tip true})]
      (is (:ok? r))
      (is (= 20N (:tip r)))
      (is (= 95N (:total r))))

    ;; $100: 20%=$20, 30%=$30 → $20 (multiple of $20)
    (let [r (ev/convert-request {:op :tip :bill 100N :round-tip true})]
      (is (:ok? r))
      (is (= 20N (:tip r)))
      (is (= 120N (:total r))))

    ;; $150: 20%=$30, 30%=$45 → $40 (multiple of $20)
    (let [r (ev/convert-request {:op :tip :bill 150N :round-tip true})]
      (is (:ok? r))
      (is (= 40N (:tip r)))
      (is (= 190N (:total r))))

    ;; $33: 20%=$6.60, 30%=$9.90 → $7 (no $20/$10/$5 fits, $1 multiple)
    (let [r (ev/convert-request {:op :tip :bill 33N :round-tip true})]
      (is (:ok? r))
      (is (= 7N (:tip r)))
      (is (= 40N (:total r))))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-tip-explicit
  (testing "X% tip on Y"
    (let [{:keys [result]} (cli/process-request-text "20 percent tip on 50" nil)]
      (is (= "Bill: $50, Tip: $10 (20%), Total: $60" result))))

  (testing "tip on Y at X%"
    (let [{:keys [result]} (cli/process-request-text "tip on $100 at 15 percent" nil)]
      (is (= "Bill: $100, Tip: $15 (15%), Total: $115" result))))

  (testing "tip with decimals"
    (let [{:keys [result]} (cli/process-request-text "18 percent tip on $85.50" nil)]
      (is (= "Bill: $85.5, Tip: $15.39 (18%), Total: $100.89" result)))))

(deftest end-to-end-round-tip
  (testing "tip $50 — round tip"
    (let [{:keys [result]} (cli/process-request-text "tip $50" nil)]
      (is (= "Bill: $50, Tip: $10 (20%), Total: $60" result))))

  (testing "tip $85 — round tip picks $20"
    (let [{:keys [result]} (cli/process-request-text "tip 85" nil)]
      (is (= "Bill: $85, Tip: $20 (23.5%), Total: $105" result))))

  (testing "tip $47 — round tip picks $10"
    (let [{:keys [result]} (cli/process-request-text "tip $47" nil)]
      (is (= "Bill: $47, Tip: $10 (21.3%), Total: $57" result)))))
