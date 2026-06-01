(ns calc.completions-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.completions :as c]))

;; ============================================================================
;; dim-label — human-readable dimension names
;; ============================================================================

(deftest dim-label-simple-test
  (testing "simple dimensions match known categories"
    (is (= "Length"      (c/dim-label {:length 1})))
    (is (= "Area"        (c/dim-label {:length 2})))
    (is (= "Volume"      (c/dim-label {:length 3})))
    (is (= "Mass"        (c/dim-label {:mass 1})))
    (is (= "Time"        (c/dim-label {:time 1})))
    (is (= "Data"        (c/dim-label {:data 1})))
    (is (= "Speed"       (c/dim-label {:length 1 :time -1})))
    (is (= "Energy"      (c/dim-label {:mass 1 :length 2 :time -2})))
    (is (= "Power"       (c/dim-label {:mass 1 :length 2 :time -3})))
    (is (= "Pressure"    (c/dim-label {:mass 1 :length -1 :time -2})))
    (is (= "Frequency"   (c/dim-label {:time -1})))))

(deftest dim-label-compound-test
  (testing "compound dimensions factor into known categories"
    (is (= "Speed / Mass"   (c/dim-label {:length 1 :time -1 :mass -1})))
    (is (= "Mass / Length"  (c/dim-label {:mass 1 :length -1})))
    (is (= "Time / Mass"    (c/dim-label {:time 1 :mass -1})))
    (is (= "Length / Data"  (c/dim-label {:length 1 :data -1})))
    (is (= "Length / Mass"  (c/dim-label {:length 1 :mass -1})))
    (is (= "Mass / Time"    (c/dim-label {:mass 1 :time -1})))
    (is (= "Area / Time^2"  (c/dim-label {:length 2 :time -2}))))
  (testing "nil for non-map inputs"
    (is (nil? (c/dim-label nil)))
    (is (nil? (c/dim-label :temperature)))))

;; ============================================================================
;; target-dim-hint — preview hints for target unit phase
;; ============================================================================

(deftest target-dim-hint-test
  (testing "shows dimension after connector"
    (is (= "Length"         (c/target-dim-hint "12 feet in ")))
    (is (= "Mass"           (c/target-dim-hint "5 kg to ")))
    (is (= "Speed"          (c/target-dim-hint "60 mph in ")))
    (is (= "Speed / Mass"   (c/target-dim-hint "34 mph/gram in ")))
    (is (= "Mass / Length"  (c/target-dim-hint "24 gram/mile in ")))
    (is (= "Time / Mass"    (c/target-dim-hint "44 millennia/kg in ")))
    (is (= "Length / Data"  (c/target-dim-hint "12 meter/bytes in "))))
  (testing "works with prefix after connector"
    (is (= "Speed / Mass"   (c/target-dim-hint "34 mph/gram in f")))
    (is (= "Length"          (c/target-dim-hint "12 feet in y"))))
  (testing "works with abutted number+unit"
    (is (= "Length"          (c/target-dim-hint "12ft in ")))
    (is (= "Speed"           (c/target-dim-hint "100mph in "))))
  (testing "nil when not in target-unit phase"
    (is (nil? (c/target-dim-hint "12 feet ")))
    (is (nil? (c/target-dim-hint "")))
    (is (nil? (c/target-dim-hint "12 ")))))

;; ============================================================================
;; complete — context-aware completions
;; ============================================================================

(deftest complete-slash-commands-test
  (testing "slash prefix completes commands"
    (let [results (c/complete "/h")]
      (is (= 1 (count results)))
      (is (= "/help" (:text (first results)))))
    (let [results (c/complete "/")]
      (is (= 8 (count results))))))

(deftest complete-connectors-test
  (testing "after a unit suggests connectors"
    (let [results (c/complete "12 feet ")]
      (is (= 2 (count results)))
      (is (= #{"in" "to"} (set (map :text results))))))
  (testing "after abutted number+unit suggests connectors"
    (let [results (c/complete "12ft ")]
      (is (= 2 (count results)))
      (is (= #{"in" "to"} (set (map :text results)))))))

(deftest complete-dimension-filtered-test
  (testing "after connector filters by source dimension"
    (let [results (c/complete "12 feet in ")]
      (is (every? #(= {:length 1} (:dim %)) results))
      (is (some #(= "meter" (:text %)) results))
      (is (not (some #(= "gram" (:text %)) results)))))
  (testing "compound source filters to compound targets"
    (let [results (c/complete "34 mph/gram in ")]
      (is (pos? (count results)))
      ;; All results should have compound group labels like "Speed / Mass"
      (is (every? #(clojure.string/includes? (:group %) " / ") results)))))


(deftest complete-compound-denom-test
  (testing "completing denominator of compound target"
    (let [results (c/complete "34 mph/gram in fps/p")]
      (is (pos? (count results)))
      (is (every? #(clojure.string/starts-with? (:text %) "fps/p") results))
      ;; Should suggest mass units (pound, pounds)
      (is (some #(= "fps/pound" (:text %)) results)))))

(deftest complete-unit-prefix-test
  (testing "prefix filters units"
    (let [results (c/complete "12 f")]
      (is (every? #(clojure.string/starts-with?
                     (clojure.string/lower-case (:text %))
                     "f")
                  results))
      (is (some #(= "feet" (:text %)) results)))))
