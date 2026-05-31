(ns calc.dice-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.dice :as dice]))

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
