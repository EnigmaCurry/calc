(ns calc.web-test
  "Tests for web-specific functionality (UI helpers, slash commands, etc.).
   Conversion/math/formatting correctness is covered by calc.shared-test."
  (:require [cljs.test :refer [deftest testing is]]
            [calc.web :as web]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; web/evaluate — web-specific behavior
;; ---------------------------------------------------------------------------

(deftest evaluate-blank-input
  (testing "blank input returns nil"
    (is (nil? (web/evaluate "" nil)))
    (is (nil? (web/evaluate "   " nil)))))

(deftest evaluate-returns-from-and-target
  (testing "conversion result includes :from and :target"
    (let [r (web/evaluate "12 feet in yards" nil)]
      (is (some? (:result r)))
      (is (some? (:from r))))))

;; ---------------------------------------------------------------------------
;; format-unit-label (web copy)
;; ---------------------------------------------------------------------------

(deftest format-unit-label-test
  (testing "simple keyword"
    (is (string? (web/format-unit-label :ft))))

  (testing "exponent map with denominator"
    (let [label (web/format-unit-label {:mi 1 :hr -1})]
      (is (string? label))
      (is (str/includes? label "/"))))

  (testing "square units"
    (let [label (web/format-unit-label {:ft 2})]
      (is (string? label))
      (is (or (str/includes? label "^2")
              (str/includes? label "²"))))))

;; ---------------------------------------------------------------------------
;; format-quantity-label (web copy)
;; ---------------------------------------------------------------------------

(deftest format-quantity-label-test
  (testing "simple quantity map"
    (let [label (web/format-quantity-label {:value 5 :unit :ft})]
      (is (string? label))
      (is (str/includes? label "5"))))

  (testing "vector of quantities (mixed)"
    (let [label (web/format-quantity-label [{:value 5 :unit :ft} {:value 11 :unit :in}])]
      (is (string? label))
      (is (str/includes? label "5"))
      (is (str/includes? label "11")))))

;; ---------------------------------------------------------------------------
;; slash command parsing
;; ---------------------------------------------------------------------------

(deftest parse-slash-command-test
  (testing "help command"
    (is (= {:cmd "help" :arg nil}
           (web/parse-slash-command "/help"))))

  (testing "precision command with arg"
    (is (= {:cmd "p" :arg "3"}
           (web/parse-slash-command "/p 3"))))

  (testing "sig-figs command"
    (is (= {:cmd "s" :arg "4"}
           (web/parse-slash-command "/s 4"))))

  (testing "non-slash returns nil"
    (is (nil? (web/parse-slash-command "hello")))))

;; ---------------------------------------------------------------------------
;; clear commands
;; ---------------------------------------------------------------------------

(deftest clear-commands-test
  (testing "recognized clear commands"
    (is (contains? web/clear-commands "clear"))
    (is (contains? web/clear-commands "/clear"))
    (is (contains? web/clear-commands "reset"))
    (is (contains? web/clear-commands "/reset")))

  (testing "non-clear commands"
    (is (not (contains? web/clear-commands "help")))
    (is (not (contains? web/clear-commands "/help")))))
