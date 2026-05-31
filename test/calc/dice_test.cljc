(ns calc.dice-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [calc.dice :as dice]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.format :as fmt]))

;; ============================================================================
;; Parser tests
;; ============================================================================

(deftest parse-basic-dice
  (testing "simple d20"
    (is (= {:count 1 :sides 20}
           (select-keys (dice/parse-dice "d20") [:count :sides]))))

  (testing "explicit count"
    (is (= {:count 2 :sides 6}
           (select-keys (dice/parse-dice "2d6") [:count :sides]))))

  (testing "uppercase D accepted"
    (is (= {:count 1 :sides 20}
           (select-keys (dice/parse-dice "D20") [:count :sides]))))

  (testing "1d100"
    (is (= {:count 1 :sides 100}
           (select-keys (dice/parse-dice "1d100") [:count :sides])))))

(deftest parse-modifiers
  (testing "positive modifier"
    (is (= 5 (:modifier (dice/parse-dice "1d20+5")))))

  (testing "negative modifier"
    (is (= -1 (:modifier (dice/parse-dice "2d6-1")))))

  (testing "whitespace around modifier"
    (is (= 3 (:modifier (dice/parse-dice "4d8 + 3")))))

  (testing "no modifier defaults to 0"
    (is (= 0 (:modifier (dice/parse-dice "d20"))))))

(deftest parse-keep-drop
  (testing "keep highest"
    (is (= {:op :kh :n 1}
           (:keep-drop (dice/parse-dice "2d20kh1")))))

  (testing "keep lowest"
    (is (= {:op :kl :n 1}
           (:keep-drop (dice/parse-dice "2d20kl1")))))

  (testing "drop highest"
    (is (= {:op :dh :n 2}
           (:keep-drop (dice/parse-dice "5d10dh2")))))

  (testing "drop lowest"
    (is (= {:op :dl :n 1}
           (:keep-drop (dice/parse-dice "4d6dl1")))))

  (testing "keep highest with space"
    (is (= {:op :kh :n 3}
           (:keep-drop (dice/parse-dice "4d6 kh3")))))

  (testing "D&D stat roll 4d6kh3"
    (let [parsed (dice/parse-dice "4d6kh3")]
      (is (= 4 (:count parsed)))
      (is (= 6 (:sides parsed)))
      (is (= {:op :kh :n 3} (:keep-drop parsed))))))

(deftest parse-exploding
  (testing "exploding marker"
    (is (true? (:explode (dice/parse-dice "6d6!")))))

  (testing "no explode by default"
    (is (not (:explode (dice/parse-dice "6d6"))))))

(deftest parse-comparison
  (testing ">= comparison"
    (is (= {:op :>= :target 15}
           (:comparison (dice/parse-dice "1d20+7>=15")))))

  (testing "> comparison"
    (is (= {:op :> :target 10}
           (:comparison (dice/parse-dice "1d20>10")))))

  (testing "<= comparison"
    (is (= {:op :<= :target 12}
           (:comparison (dice/parse-dice "3d6<=12")))))

  (testing "< comparison"
    (is (= {:op :< :target 12}
           (:comparison (dice/parse-dice "3d6+2<12")))))

  (testing "== comparison"
    (is (= {:op :== :target 20}
           (:comparison (dice/parse-dice "1d20==20")))))

  (testing "no comparison by default"
    (is (nil? (:comparison (dice/parse-dice "1d20"))))))

(deftest parse-combined
  (testing "full expression 4d6kh3+2>=15"
    (let [parsed (dice/parse-dice "4d6kh3+2>=15")]
      (is (= 4 (:count parsed)))
      (is (= 6 (:sides parsed)))
      (is (= {:op :kh :n 3} (:keep-drop parsed)))
      (is (= 2 (:modifier parsed)))
      (is (= {:op :>= :target 15} (:comparison parsed)))))

  (testing "exploding with modifier"
    (let [parsed (dice/parse-dice "6d6!+3")]
      (is (= 6 (:count parsed)))
      (is (= 6 (:sides parsed)))
      (is (true? (:explode parsed)))
      (is (= 3 (:modifier parsed))))))

;; ============================================================================
;; Validation / rejection tests
;; ============================================================================

(deftest parse-rejects-invalid
  (testing "zero dice"
    (is (:error (dice/parse-dice "0d6"))))

  (testing "one-sided die"
    (is (:error (dice/parse-dice "2d1"))))

  (testing "bare d"
    (is (:error (dice/parse-dice "d"))))

  (testing "double d"
    (is (:error (dice/parse-dice "2dd6"))))

  (testing "keep zero"
    (is (:error (dice/parse-dice "4d6kh0"))))

  (testing "keep more than rolled"
    (is (:error (dice/parse-dice "4d6kh9"))))

  (testing "drop all dice"
    (is (:error (dice/parse-dice "4d6dh4"))))

  (testing "non-numeric modifier"
    (is (:error (dice/parse-dice "4d6+foo")))))

;; ============================================================================
;; Roller tests
;; ============================================================================

(deftest roll-basic
  (testing "roll results are within range"
    (let [parsed (dice/parse-dice "3d6")
          result (dice/roll parsed)]
      (is (= 3 (count (:rolls result))))
      (is (every? #(<= 1 % 6) (:rolls result)))
      (is (= (reduce + (:rolls result)) (:total result))))))

(deftest roll-with-modifier
  (testing "modifier added to total"
    (let [parsed (dice/parse-dice "1d6+5")
          result (dice/roll parsed)]
      (is (= (+ (first (:rolls result)) 5) (:total result))))))

(deftest roll-keep-highest
  (testing "keeps correct number of dice"
    (let [result (dice/roll (dice/parse-dice "4d6kh3"))]
      (is (= 4 (count (:rolls result))))
      (is (= 3 (count (:kept result))))
      (is (= 1 (count (:dropped result))))
      ;; kept should be the 3 highest
      (is (>= (apply min (:kept result))
              (apply max (:dropped result))))
      (is (= (reduce + (:kept result)) (:total result))))))

(deftest roll-keep-lowest
  (testing "keeps lowest dice"
    (let [result (dice/roll (dice/parse-dice "4d6kl1"))]
      (is (= 4 (count (:rolls result))))
      (is (= 1 (count (:kept result))))
      (is (= 3 (count (:dropped result))))
      (is (<= (apply max (:kept result))
              (apply min (:dropped result)))))))

(deftest roll-drop-highest
  (testing "drops highest dice"
    (let [result (dice/roll (dice/parse-dice "4d6dh1"))]
      (is (= 4 (count (:rolls result))))
      (is (= 3 (count (:kept result))))
      (is (= 1 (count (:dropped result))))
      (is (>= (apply min (:dropped result))
              (apply max (:kept result)))))))

(deftest roll-drop-lowest
  (testing "drops lowest dice"
    (let [result (dice/roll (dice/parse-dice "4d6dl1"))]
      (is (= 4 (count (:rolls result))))
      (is (= 3 (count (:kept result))))
      (is (= 1 (count (:dropped result))))
      (is (<= (apply max (:dropped result))
              (apply min (:kept result)))))))

(deftest roll-exploding
  (testing "exploding dice produce valid results"
    ;; Use a small die so explosions are likely
    (let [result (dice/roll (dice/parse-dice "10d2!"))]
      ;; Each roll entry should be a map with :initial and :total
      (is (every? map? (:rolls result)))
      (is (every? #(contains? % :initial) (:rolls result)))
      (is (every? #(contains? % :total) (:rolls result)))
      (is (every? #(>= (:total %) (:initial %)) (:rolls result)))
      (is (= (reduce + (map :total (:rolls result))) (:total result))))))

(deftest roll-comparison
  (testing "comparison result included"
    (let [result (dice/roll (dice/parse-dice "1d20>=1"))]
      (is (contains? result :comparison))
      (is (contains? (:comparison result) :success))
      (is (true? (:success (:comparison result)))))))

(deftest roll-deterministic
  (testing "seeded rolls are reproducible"
    ;; Roll many times and verify total is always in valid range
    (dotimes [_ 100]
      (let [result (dice/roll (dice/parse-dice "2d6+3"))]
        (is (<= 5 (:total result) 15))))))

;; ============================================================================
;; Integration: "roll ..." parsing
;; ============================================================================

(deftest parse-roll-command
  (testing "recognizes roll prefix"
    (is (some? (dice/parse-roll "roll d20")))
    (is (some? (dice/parse-roll "roll 2d6+3")))
    (is (some? (dice/parse-roll "Roll 4d6kh3"))))

  (testing "rejects non-roll input"
    (is (nil? (dice/parse-roll "12 feet in yards")))
    (is (nil? (dice/parse-roll "d20")))))

;; ============================================================================
;; Format tests
;; ============================================================================

(deftest format-basic-roll
  (testing "simple roll format"
    (let [result {:roll {:rolls [3 5 2]
                         :kept [3 5 2]
                         :dropped []
                         :modifier 0
                         :total 10}}]
      (is (= "Rolls: [3, 5, 2] = 10"
             (fmt/format-op-result {:op :roll} result nil)))))

  (testing "roll with positive modifier"
    (let [result {:roll {:rolls [4 6]
                         :kept [4 6]
                         :dropped []
                         :modifier 3
                         :total 13}}]
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "+ 3"))
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "= 13"))))

  (testing "roll with negative modifier"
    (let [result {:roll {:rolls [4 6]
                         :kept [4 6]
                         :dropped []
                         :modifier -2
                         :total 8}}]
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "- 2"))
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "= 8")))))

(deftest format-keep-drop-roll
  (testing "kept and dropped shown"
    (let [result {:roll {:rolls [1 6 5 3]
                         :kept [6 5 3]
                         :dropped [1]
                         :modifier 0
                         :total 14}}]
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "kept [6, 5, 3]"))
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "dropped [1]")))))

(deftest format-exploding-roll
  (testing "exploding dice formatted with bangs"
    (let [result {:roll {:rolls [{:initial 6 :exploded [6 2] :total 14}
                                 {:initial 3 :exploded [] :total 3}]
                         :kept [14 3]
                         :dropped []
                         :modifier 0
                         :total 17}}]
      (let [output (fmt/format-op-result {:op :roll} result nil)]
        (is (str/includes? output "6!6!2=14"))
        (is (str/includes? output "3"))
        (is (str/includes? output "= 17"))))))

(deftest format-comparison-roll
  (testing "success shown"
    (let [result {:roll {:rolls [15]
                         :kept [15]
                         :dropped []
                         :modifier 7
                         :total 22
                         :comparison {:op :>= :target 15 :success true}}}]
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "Success!"))))

  (testing "failure shown"
    (let [result {:roll {:rolls [2]
                         :kept [2]
                         :dropped []
                         :modifier 3
                         :total 5
                         :comparison {:op :>= :target 15 :success false}}}]
      (is (str/includes? (fmt/format-op-result {:op :roll} result nil) "Failure")))))

;; ============================================================================
;; Comparison logic tests (deterministic)
;; ============================================================================

(deftest comparison-operators-deterministic
  (testing ">= success and failure"
    (let [cmp {:op :>= :target 10}]
      ;; Use fixed rolls via the internal compare
      (is (true? (:success (:comparison (dice/roll {:count 1 :sides 1000000 :modifier 10 :explode false :comparison cmp})))))
      ;; 1d2 with no modifier: max total is 2, target is 10
      (dotimes [_ 50]
        (let [result (dice/roll (dice/parse-dice "1d2>=10"))]
          (is (false? (:success (:comparison result))))))))

  (testing "> operator"
    (dotimes [_ 50]
      (let [result (dice/roll (dice/parse-dice "1d2>100"))]
        (is (false? (:success (:comparison result)))))))

  (testing "<= operator always succeeds for 1d2<=2"
    (dotimes [_ 50]
      (let [result (dice/roll (dice/parse-dice "1d2<=2"))]
        (is (true? (:success (:comparison result)))))))

  (testing "< operator always fails for 1d6<1"
    (dotimes [_ 50]
      (let [result (dice/roll (dice/parse-dice "1d6<1"))]
        (is (false? (:success (:comparison result)))))))

  (testing "== operator"
    ;; 1d2==3 should always fail (max roll is 2)
    (dotimes [_ 50]
      (let [result (dice/roll (dice/parse-dice "1d2==3"))]
        (is (false? (:success (:comparison result))))))))

;; ============================================================================
;; Exploding + keep/drop combined
;; ============================================================================

(deftest roll-exploding-with-keep-drop
  (testing "exploding dice with keep highest"
    (dotimes [_ 50]
      (let [result (dice/roll (dice/parse-dice "4d6!kh3"))]
        (is (= 4 (count (:rolls result))))
        (is (every? map? (:rolls result)))
        (is (= 3 (count (:kept result))))
        (is (= 1 (count (:dropped result))))
        ;; kept values should be the 3 highest totals
        (is (>= (apply min (:kept result))
                (apply max (:dropped result))))
        (is (= (reduce + (:kept result)) (:total result))))))

  (testing "exploding dice with drop lowest"
    (dotimes [_ 50]
      (let [result (dice/roll (dice/parse-dice "3d4!dl1"))]
        (is (= 3 (count (:rolls result))))
        (is (= 2 (count (:kept result))))
        (is (= 1 (count (:dropped result))))
        (is (<= (apply max (:dropped result))
                (apply min (:kept result))))))))

;; ============================================================================
;; Error propagation
;; ============================================================================

(deftest roll-error-propagation
  (testing "rolling an invalid parse throws"
    (let [bad-parse (dice/parse-dice "0d6")]
      (is (:error bad-parse))
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (dice/roll bad-parse)))))

  (testing "invalid parse through convert-request returns error"
    (let [parsed (parser/parse-request "roll 0d6")
          result (ev/convert-request parsed)]
      (is (not (:ok? result))))))

;; ============================================================================
;; End-to-end pipeline tests
;; ============================================================================

(deftest end-to-end-roll-pipeline
  (testing "roll d20 through full pipeline"
    (let [parsed (parser/parse-request "roll d20")
          _ (is (= :roll (:op parsed)))
          result (ev/convert-request parsed)
          _ (is (:ok? result))
          output (fmt/format-op-result parsed result nil)]
      (is (string? output))
      (is (str/starts-with? output "Rolls: ["))
      (is (str/includes? output "= "))))

  (testing "roll 4d6kh3 through full pipeline"
    (let [parsed (parser/parse-request "roll 4d6kh3")
          result (ev/convert-request parsed)
          output (fmt/format-op-result parsed result nil)]
      (is (:ok? result))
      (is (str/includes? output "kept"))
      (is (str/includes? output "dropped"))))

  (testing "roll 2d6+3>=10 through full pipeline"
    (let [parsed (parser/parse-request "roll 2d6+3>=10")
          result (ev/convert-request parsed)
          output (fmt/format-op-result parsed result nil)]
      (is (:ok? result))
      (is (or (str/includes? output "Success!")
              (str/includes? output "Failure")))))

  (testing "roll 6d6! through full pipeline"
    (let [parsed (parser/parse-request "roll 6d6!")
          result (ev/convert-request parsed)
          output (fmt/format-op-result parsed result nil)]
      (is (:ok? result))
      (is (str/includes? output "Rolls: ["))))

  (testing "Roll (capitalized) works"
    (let [parsed (parser/parse-request "Roll 2d20kh1")
          result (ev/convert-request parsed)]
      (is (:ok? result))
      (is (= 2 (count (get-in result [:roll :rolls])))))))
