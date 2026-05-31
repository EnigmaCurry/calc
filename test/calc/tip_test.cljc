(ns calc.tip-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.format :as fmt]
            [calc.test-helpers :as th]))

(defn tip-output
  "Parse a tip expression, evaluate, and format the full output string."
  [input]
  (let [parsed (parser/parse-request input)
        result (ev/convert-request parsed)]
    (assert (:ok? result) (str "tip eval failed: " (pr-str result)))
    (fmt/format-op-result parsed result nil)))

;; ---------------------------------------------------------------------------
;; Parser: parse-tip produces correct request maps
;; ---------------------------------------------------------------------------

(deftest parse-tip-bill-and-percent
  (testing "tip N M — bill then percent"
    (let [r (parser/parse-tip "tip 45 10")]
      (is (= :tip (:op r)))
      (is (== 45 (:bill r)))
      (is (== 10 (:percent r)))))

  (testing "tip $N M — dollar bill then percent"
    (let [r (parser/parse-tip "tip $45 10")]
      (is (= :tip (:op r)))
      (is (== 45 (:bill r)))
      (is (== 10 (:percent r)))))

  (testing "tip $N M% — dollar bill then percent with % (via parse-request)"
    (let [r (parser/parse-request "tip $45 10%")]
      (is (= :tip (:op r)))
      (is (== 45 (:bill r)))
      (is (== 10 (:percent r)))))

  (testing "tip M% $N — percent then dollar bill swapped (via parse-request)"
    (let [r (parser/parse-request "tip 10% $45")]
      (is (= :tip (:op r)))
      (is (== 45 (:bill r)))
      (is (== 10 (:percent r))))))

(deftest parse-tip-round-tip
  (testing "tip N — single number, round tip table"
    (let [r (parser/parse-tip "tip 45")]
      (is (= :tip (:op r)))
      (is (== 45 (:bill r)))
      (is (true? (:round-tip r)))))

  (testing "tip $N — dollar amount, round tip table"
    (let [r (parser/parse-tip "tip $50")]
      (is (= :tip (:op r)))
      (is (== 50 (:bill r)))
      (is (true? (:round-tip r))))))

(deftest parse-tip-target-total
  (testing "tip $N $M — two dollar amounts, bill then target total"
    (let [r (parser/parse-tip "tip $43 $55")]
      (is (= :tip (:op r)))
      (is (== 43 (:bill r)))
      (is (true? (:exact r)))
      (is (number? (:percent r))))))

(deftest parse-tip-verbose-forms
  (testing "N% tip on $M"
    (let [r (parser/parse-tip "20 percent tip on $50")]
      (is (= :tip (:op r)))
      (is (== 50 (:bill r)))
      (is (== 20 (:percent r)))))

  (testing "tip on $M at N%"
    (let [r (parser/parse-tip "tip on $85.50 at 18 percent")]
      (is (= :tip (:op r)))
      (is (== 85.5 (:bill r)))
      (is (== 18 (:percent r)))))

  (testing "what is the tip on $M at N%"
    (let [r (parser/parse-tip "what is the tip on $100 at 15 percent")]
      (is (= :tip (:op r)))
      (is (== 100 (:bill r)))
      (is (== 15 (:percent r))))))

;; ---------------------------------------------------------------------------
;; Formatted output: verify the rendered tip table
;; ---------------------------------------------------------------------------

(deftest tip-output-explicit-percent
  (testing "tip 45 10 — 10% of $45"
    (let [out (tip-output "tip 45 10")]
      (is (str/includes? out "Bill"))
      (is (str/includes? out "$45.00"))
      (is (str/includes? out "10%"))
      (is (str/includes? out "$4.50"))))

  (testing "tip $100 15 — 15% of $100"
    (let [out (tip-output "tip $100 15")]
      (is (str/includes? out "$100.00"))
      (is (str/includes? out "15%"))
      (is (str/includes? out "$15.00")))))

(deftest tip-output-swapped-order
  (testing "tip 10% $45 — percent first, dollar bill second"
    (let [out (tip-output "tip 10% $45")]
      (is (str/includes? out "$45.00"))
      (is (str/includes? out "10%"))
      (is (str/includes? out "$4.50")))))

(deftest tip-output-round-tip-table
  (testing "tip 45 — full table with 15%, 20%, round options"
    (let [out (tip-output "tip 45")]
      (is (str/includes? out "Bill"))
      (is (str/includes? out "$45.00"))
      (is (str/includes? out "15%"))
      (is (str/includes? out "20%"))
      ;; Should have multiple rows
      (is (> (count (str/split-lines out)) 2)))))

(deftest tip-output-dollar-round-tip
  (testing "tip $50 — dollar amount, full table"
    (let [out (tip-output "tip $50")]
      (is (str/includes? out "$50.00"))
      (is (str/includes? out "15%"))
      (is (str/includes? out "20%")))))

(deftest tip-output-target-total
  (testing "tip $43 $55 — target total, percentage should be rounded"
    (let [out (tip-output "tip $43 $55")]
      (is (str/includes? out "$43.00"))
      (is (str/includes? out "$55.00"))
      (is (str/includes? out "$12.00"))
      ;; Percentage label should be rounded (27.9%), not raw (27.9069767442%)
      (is (str/includes? out "27.9%"))
      (is (not (str/includes? out "27.9069"))))))

(deftest tip-output-exact-percent
  (testing "tip $50 $60 — exact 20%"
    (let [out (tip-output "tip $50 $60")]
      (is (str/includes? out "20%"))
      (is (str/includes? out "$10.00"))
      (is (str/includes? out "$60.00")))))

;; ---------------------------------------------------------------------------
;; End-to-end tests via evaluate helper
;; ---------------------------------------------------------------------------

(deftest end-to-end-tip-explicit
  (testing "explicit percent shows exact + round tip"
    (let [{:keys [result]} (th/evaluate "tip $85 18%" nil)]
      (is (re-find #"\$85\.00" result))
      (is (re-find #"18%" result)))))

(deftest end-to-end-tip-two-dollar-amounts
  (testing "$40 bill with $55 total — single exact row"
    (let [{:keys [result]} (th/evaluate "tip $40 $55" nil)]
      (is (re-find #"\$40\.00" result))
      (is (re-find #"\$15\.00" result))
      (is (re-find #"\$55\.00" result))
      (is (re-find #"37\.5%" result))
      ;; Only one data row (plus the Bill line)
      (is (= 2 (count (str/split-lines result)))))))

(deftest end-to-end-round-tip
  (testing "$85 shows 15%, 20%, and round-amount rows"
    (let [{:keys [result]} (th/evaluate "tip $85" nil)]
      (is (re-find #"\$85\.00" result))
      (is (re-find #"15%" result))
      (is (re-find #"20%" result))))

  (testing "tip $50 table"
    (let [{:keys [result]} (th/evaluate "tip $50" nil)]
      (is (re-find #"\$50\.00" result))
      (is (re-find #"\$10\.00" result)))))

(deftest tip-money-format-two-decimals
  (testing "tip amounts always show exactly two decimal places"
    (let [{:keys [result]} (th/evaluate "tip $42" nil)]
      (is (re-find #"\$42\.00" result))
      (is (re-find #"\$6\.30" result))
      (is (re-find #"\$8\.40" result))))

  (testing "whole dollar tips still show .00"
    (let [{:keys [result]} (th/evaluate "tip $50" nil)]
      (is (re-find #"\$7\.50" result))
      (is (re-find #"\$10\.00" result))))

  (testing "penny-precise tip amounts"
    (let [{:keys [result]} (th/evaluate "tip $29.99 18%" nil)]
      (is (re-find #"\$29\.99" result))
      (is (re-find #"\$5\.40" result)))))

(deftest tip-column-alignment
  (testing "all colons are aligned in output rows"
    (let [{:keys [result]} (th/evaluate "tip $85" nil)
          lines (str/split-lines result)
          colon-positions (map #(str/index-of % ":") lines)]
      (is (apply = colon-positions)
          (str "Colons not aligned: " (pr-str (zipmap lines colon-positions))))))

  (testing "decimal points are aligned in tip and total columns"
    (let [{:keys [result]} (th/evaluate "tip $42" nil)
          lines (str/split-lines result)
          tip-dots (keep #(let [i (str/index-of % "Tip")]
                            (when (and i (pos? i))
                              (str/index-of % "." i)))
                         lines)
          total-dots (keep #(let [i (str/index-of % "Total")]
                              (when (and i (pos? i))
                                (str/index-of % "." i)))
                           lines)]
      (is (apply = tip-dots)
          (str "Tip decimals not aligned: " tip-dots))
      (is (apply = total-dots)
          (str "Total decimals not aligned: " total-dots)))))
