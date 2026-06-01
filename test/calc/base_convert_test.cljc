(ns calc.base-convert-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.test-helpers :as th]))

;; ==========================================================================
;; Parser tests — base-prefixed literals
;; ==========================================================================

(deftest parses-hex-literals
  (testing "0x prefix recognized as hex input"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "0xff in decimal"
      {:op :base-convert :value 255 :from-base :hex :to-base :decimal}

      "0xFF in decimal"
      {:op :base-convert :value 255 :from-base :hex :to-base :decimal}

      "0x1A in binary"
      {:op :base-convert :value 26 :from-base :hex :to-base :binary})))

(deftest parses-binary-literals
  (testing "0b prefix recognized as binary input"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "0b11111111 in decimal"
      {:op :base-convert :value 255 :from-base :binary :to-base :decimal}

      "0b1010 in hex"
      {:op :base-convert :value 10 :from-base :binary :to-base :hex}

      "0b0 in decimal"
      {:op :base-convert :value 0 :from-base :binary :to-base :decimal})))

(deftest parses-octal-literals
  (testing "0o prefix recognized as octal input"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "0o377 in decimal"
      {:op :base-convert :value 255 :from-base :octal :to-base :decimal}

      "0o10 in decimal"
      {:op :base-convert :value 8 :from-base :octal :to-base :decimal})))

;; ==========================================================================
;; Parser tests — base name suffix on value
;; ==========================================================================

(deftest parses-base-name-suffix
  (testing "value followed by base name"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "ff hex in decimal"
      {:op :base-convert :value 255 :from-base :hex :to-base :decimal}

      "11111111 binary in decimal"
      {:op :base-convert :value 255 :from-base :binary :to-base :decimal}

      "377 octal in decimal"
      {:op :base-convert :value 255 :from-base :octal :to-base :decimal}

      "ff hexadecimal in decimal"
      {:op :base-convert :value 255 :from-base :hex :to-base :decimal})))

;; ==========================================================================
;; Parser tests — decimal to various bases
;; ==========================================================================

(deftest parses-decimal-to-bases
  (testing "decimal number to named base"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "255 in hex"
      {:op :base-convert :value 255 :from-base :decimal :to-base :hex}

      "255 in hexadecimal"
      {:op :base-convert :value 255 :from-base :decimal :to-base :hex}

      "255 in binary"
      {:op :base-convert :value 255 :from-base :decimal :to-base :binary}

      "255 in octal"
      {:op :base-convert :value 255 :from-base :decimal :to-base :octal}

      "255 to hex"
      {:op :base-convert :value 255 :from-base :decimal :to-base :hex}

      "convert 255 to binary"
      {:op :base-convert :value 255 :from-base :decimal :to-base :binary}

      "what is 255 in hex"
      {:op :base-convert :value 255 :from-base :decimal :to-base :hex})))

;; ==========================================================================
;; Parser tests — arbitrary bases
;; ==========================================================================

(deftest parses-arbitrary-base-targets
  (testing "'in base N' target"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "255 in base 2"
      {:op :base-convert :value 255 :from-base :decimal :to-base 2}

      "255 in base 16"
      {:op :base-convert :value 255 :from-base :decimal :to-base 16}

      "255 in base 8"
      {:op :base-convert :value 255 :from-base :decimal :to-base 8}

      "255 in base 7"
      {:op :base-convert :value 255 :from-base :decimal :to-base 7}

      "100 in base 3"
      {:op :base-convert :value 100 :from-base :decimal :to-base 3})))

;; ==========================================================================
;; Parser tests — sexagesimal (colon-separated H:M:S)
;; ==========================================================================

(deftest parses-sexagesimal-input
  (testing "colon-separated input recognized as sexagesimal"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "1:30:45 in decimal"
      {:op :base-convert :value 5445 :from-base :sexagesimal :to-base :decimal}

      "1:00:00 in decimal"
      {:op :base-convert :value 3600 :from-base :sexagesimal :to-base :decimal}

      "0:01:00 in decimal"
      {:op :base-convert :value 60 :from-base :sexagesimal :to-base :decimal})))

(deftest parses-decimal-to-sexagesimal
  (testing "decimal to sexagesimal"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "5445 in sexagesimal"
      {:op :base-convert :value 5445 :from-base :decimal :to-base :sexagesimal}

      "3600 in sexagesimal"
      {:op :base-convert :value 3600 :from-base :decimal :to-base :sexagesimal})))

;; ==========================================================================
;; Parser tests — sentence forms
;; ==========================================================================

(deftest parses-question-forms
  (testing "how many / what is forms"
    (is (th/deep== {:op :base-convert :value 255 :from-base :decimal :to-base :hex}
                   (parser/parse-request "what is 255 in hex")))
    (is (th/deep== {:op :base-convert :value 255 :from-base :hex :to-base :decimal}
                   (parser/parse-request "what is 0xff in decimal")))))

;; ==========================================================================
;; Eval tests — decimal to other bases
;; ==========================================================================

(deftest evaluates-decimal-to-hex
  (testing "decimal to hex"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :decimal :to-base :hex})]
      (is (:ok? r))
      (is (= "0xff" (:value r))))))

(deftest evaluates-decimal-to-binary
  (testing "decimal to binary"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :decimal :to-base :binary})]
      (is (:ok? r))
      (is (= "0b11111111" (:value r))))))

(deftest evaluates-decimal-to-octal
  (testing "decimal to octal"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :decimal :to-base :octal})]
      (is (:ok? r))
      (is (= "0o377" (:value r))))))

(deftest evaluates-decimal-to-decimal
  (testing "decimal to decimal is identity"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :decimal :to-base :decimal})]
      (is (:ok? r))
      (is (= "255" (:value r))))))

;; ==========================================================================
;; Eval tests — other bases to decimal
;; ==========================================================================

(deftest evaluates-hex-to-decimal
  (testing "hex to decimal"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :hex :to-base :decimal})]
      (is (:ok? r))
      (is (= "255" (:value r))))))

(deftest evaluates-binary-to-decimal
  (testing "binary to decimal"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :binary :to-base :decimal})]
      (is (:ok? r))
      (is (= "255" (:value r))))))

(deftest evaluates-octal-to-decimal
  (testing "octal to decimal"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :octal :to-base :decimal})]
      (is (:ok? r))
      (is (= "255" (:value r))))))

;; ==========================================================================
;; Eval tests — cross-base conversions (non-decimal intermediary)
;; ==========================================================================

(deftest evaluates-binary-to-hex
  (testing "binary to hex"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :binary :to-base :hex})]
      (is (:ok? r))
      (is (= "0xff" (:value r))))))

(deftest evaluates-hex-to-binary
  (testing "hex to binary"
    (let [r (ev/convert-request {:op :base-convert :value 26 :from-base :hex :to-base :binary})]
      (is (:ok? r))
      (is (= "0b11010" (:value r))))))

;; ==========================================================================
;; Eval tests — arbitrary bases
;; ==========================================================================

(deftest evaluates-arbitrary-base
  (testing "decimal to base 7"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :decimal :to-base 7})]
      (is (:ok? r))
      (is (= "513" (:value r)))))

  (testing "decimal to base 3"
    (let [r (ev/convert-request {:op :base-convert :value 100 :from-base :decimal :to-base 3})]
      (is (:ok? r))
      (is (= "10201" (:value r)))))

  (testing "decimal to base 36"
    (let [r (ev/convert-request {:op :base-convert :value 255 :from-base :decimal :to-base 36})]
      (is (:ok? r))
      (is (= "73" (:value r))))))

;; ==========================================================================
;; Eval tests — sexagesimal
;; ==========================================================================

(deftest evaluates-decimal-to-sexagesimal
  (testing "decimal to sexagesimal"
    (let [r (ev/convert-request {:op :base-convert :value 5445 :from-base :decimal :to-base :sexagesimal})]
      (is (:ok? r))
      (is (= "1:30:45" (:value r)))))

  (testing "exact hour"
    (let [r (ev/convert-request {:op :base-convert :value 3600 :from-base :decimal :to-base :sexagesimal})]
      (is (:ok? r))
      (is (= "1:00:00" (:value r)))))

  (testing "zero"
    (let [r (ev/convert-request {:op :base-convert :value 0 :from-base :decimal :to-base :sexagesimal})]
      (is (:ok? r))
      (is (= "0:00:00" (:value r))))))

(deftest evaluates-sexagesimal-to-decimal
  (testing "sexagesimal to decimal"
    (let [r (ev/convert-request {:op :base-convert :value 5445 :from-base :sexagesimal :to-base :decimal})]
      (is (:ok? r))
      (is (= "5445" (:value r))))))

;; ==========================================================================
;; Eval tests — large numbers (BigInteger)
;; ==========================================================================

(deftest evaluates-large-numbers
  (testing "large decimal to hex"
    (let [r (ev/convert-request {:op :base-convert :value 4294967295 :from-base :decimal :to-base :hex})]
      (is (:ok? r))
      (is (= "0xffffffff" (:value r)))))

  (testing "large decimal to binary"
    (let [r (ev/convert-request {:op :base-convert :value 256 :from-base :decimal :to-base :binary})]
      (is (:ok? r))
      (is (= "0b100000000" (:value r))))))

;; ==========================================================================
;; Eval tests — zero and edge cases
;; ==========================================================================

(deftest evaluates-zero
  (testing "zero in all bases"
    (are [to-base expected] (let [r (ev/convert-request {:op :base-convert :value 0 :from-base :decimal :to-base to-base})]
                              (and (:ok? r) (= expected (:value r))))
      :hex "0x0"
      :binary "0b0"
      :octal "0o0"
      :decimal "0"
      :sexagesimal "0:00:00")))

;; ==========================================================================
;; End-to-end tests
;; ==========================================================================

(deftest end-to-end-decimal-to-hex
  (testing "255 in hex"
    (let [{:keys [result]} (th/evaluate "255 in hex" nil)]
      (is (= "0xff" result))))

  (testing "0xff in decimal"
    (let [{:keys [result]} (th/evaluate "0xff in decimal" nil)]
      (is (= "255" result)))))

(deftest end-to-end-decimal-to-binary
  (testing "255 in binary"
    (let [{:keys [result]} (th/evaluate "255 in binary" nil)]
      (is (= "0b11111111" result))))

  (testing "0b11111111 in decimal"
    (let [{:keys [result]} (th/evaluate "0b11111111 in decimal" nil)]
      (is (= "255" result)))))

(deftest end-to-end-decimal-to-octal
  (testing "255 in octal"
    (let [{:keys [result]} (th/evaluate "255 in octal" nil)]
      (is (= "0o377" result))))

  (testing "0o377 in decimal"
    (let [{:keys [result]} (th/evaluate "0o377 in decimal" nil)]
      (is (= "255" result)))))

(deftest end-to-end-arbitrary-base
  (testing "255 in base 7"
    (let [{:keys [result]} (th/evaluate "255 in base 7" nil)]
      (is (= "513" result)))))

(deftest end-to-end-sexagesimal
  (testing "5445 in sexagesimal"
    (let [{:keys [result]} (th/evaluate "5445 in sexagesimal" nil)]
      (is (= "1:30:45" result))))

  (testing "1:30:45 in decimal"
    (let [{:keys [result]} (th/evaluate "1:30:45 in decimal" nil)]
      (is (= "5445" result)))))

(deftest end-to-end-cross-base
  (testing "0xff in binary"
    (let [{:keys [result]} (th/evaluate "0xff in binary" nil)]
      (is (= "0b11111111" result))))

  (testing "0b1010 in hex"
    (let [{:keys [result]} (th/evaluate "0b1010 in hex" nil)]
      (is (= "0xa" result)))))

(deftest end-to-end-hex-suffix
  (testing "ff hex in decimal"
    (let [{:keys [result]} (th/evaluate "ff hex in decimal" nil)]
      (is (= "255" result)))))

(deftest end-to-end-sentence-forms
  (testing "convert form"
    (let [{:keys [result]} (th/evaluate "convert 255 to hex" nil)]
      (is (= "0xff" result))))

  (testing "what is form"
    (let [{:keys [result]} (th/evaluate "what is 255 in binary" nil)]
      (is (= "0b11111111" result)))))
