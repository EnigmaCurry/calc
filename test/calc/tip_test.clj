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
           (parser/parse-request "tip on 50"))))

  (testing "'tip for Y' triggers round-tip"
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip for $50"))))

  (testing "'tip $Y' triggers round-tip"
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip $50"))))

  (testing "'tip N' triggers round-tip"
    (is (= {:op :tip :bill 50N :round-tip true}
           (parser/parse-request "tip 50")))))

;; ==========================================================================
;; Parser tests — brief/shorthand tip forms (explicit percent)
;; ==========================================================================

(deftest parses-tip-brief-percent-bill
  (testing "'tip X% $Y' — percent then bill"
    (is (= {:op :tip :percent 10N :bill 50N}
           (parser/parse-request "tip 10% $50")))))

(deftest parses-tip-brief-bill-percent
  (testing "'tip $Y X%' — dollar bill then percent"
    (is (= {:op :tip :percent 10N :bill 50N}
           (parser/parse-request "tip $50 10%"))))

  (testing "'tip $Y N' — dollar bill then bare number percent"
    (is (= {:op :tip :percent 30N :bill 500N}
           (parser/parse-request "tip $500 30"))))

  (testing "'tip N X%' — bare bill then percent"
    (is (= {:op :tip :percent 30N :bill 500N}
           (parser/parse-request "tip 500 30%")))))

(deftest parses-tip-brief-two-bare-numbers
  (testing "'tip N M' — first is bill, second is percent"
    (is (= {:op :tip :percent 25N :bill 50N}
           (parser/parse-request "tip 50 25")))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-tip-explicit-percent
  (testing "explicit percent returns rows with exact, round tip, and round total"
    (let [r (ev/convert-request {:op :tip :percent 20N :bill 50N})
          rows (:rows r)]
      (is (:ok? r))
      ;; First row: exact 20%
      (is (= 10N (:tip (first rows))))
      (is (= 60N (:total (first rows))))
      (is (= "20%" (:label (first rows))))))

  (testing "penny rounding still applies"
    (let [r (ev/convert-request {:op :tip :percent 18N :bill 29.99M})
          exact (first (:rows r))]
      (is (:ok? r))
      (is (= 5.40M (:tip exact))))))

(deftest evaluates-round-tip-table
  (testing "$85: 15%, 20%, round tip, round total (all different totals)"
    (let [r (ev/convert-request {:op :tip :bill 85N :round-tip true})
          rows (:rows r)]
      (is (:ok? r))
      (is (= 4 (count rows)))
      ;; 15% row
      (is (= "15%" (:label (nth rows 0))))
      (is (= 12.75M (:tip (nth rows 0))))
      ;; 20% row
      (is (= "20%" (:label (nth rows 1))))
      (is (= 17N (:tip (nth rows 1))))
      ;; Round tip row (includes percent in label)
      (is (re-find #"Round tip" (:label (nth rows 2))))
      (is (= 20N (:tip (nth rows 2))))
      (is (= 105N (:total (nth rows 2))))
      ;; Round total row
      (is (re-find #"Round total" (:label (nth rows 3))))
      (is (= 25N (:tip (nth rows 3))))
      (is (= 110N (:total (nth rows 3))))))

  (testing "$50: round options deduped with 20% row (same total $60)"
    (let [r (ev/convert-request {:op :tip :bill 50N :round-tip true})
          rows (:rows r)]
      ;; Only 15% and 20% — round tip/total both give $60, same as 20%
      (is (= 2 (count rows)))
      (is (= "15%" (:label (first rows))))
      (is (= "20%" (:label (second rows))))
      (is (= 60N (:total (second rows))))))

  (testing "$120: round tip and round total converge, deduped to one"
    (let [r (ev/convert-request {:op :tip :bill 120N :round-tip true})
          rows (:rows r)]
      ;; 15%, 20%, and one round row ($150)
      (is (= 3 (count rows)))
      (is (re-find #"Round tip" (:label (nth rows 2))))
      (is (= 30N (:tip (nth rows 2))))
      (is (= 150N (:total (nth rows 2)))))))

;; ==========================================================================
;; End-to-end CLI tests
;; ==========================================================================

(deftest end-to-end-tip-explicit
  (testing "explicit percent shows exact + round tip"
    (let [{:keys [result]} (cli/process-request-text "tip $85 18%" nil)]
      (is (re-find #"Bill: \$85" result))
      (is (re-find #"18%" result))
      (is (re-find #"Round tip" result)))))

(deftest end-to-end-round-tip
  (testing "$85 shows 15%, 20%, round tip, round total"
    (let [{:keys [result]} (cli/process-request-text "tip $85" nil)]
      (is (re-find #"Bill: \$85" result))
      (is (re-find #"15%" result))
      (is (re-find #"20%" result))
      (is (re-find #"Round tip" result))
      (is (re-find #"Round total" result))))

  (testing "tip $50 table"
    (let [{:keys [result]} (cli/process-request-text "tip $50" nil)]
      (is (re-find #"Bill: \$50" result))
      (is (re-find #"Tip \$10" result)))))
