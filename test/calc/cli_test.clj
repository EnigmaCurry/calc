(ns calc.cli-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [calc.cli :as cli]))

(deftest math-expressions-via-process-request-text
  (testing "bare number"
    (let [{:keys [result error]} (cli/process-request-text "2" nil)]
      (is (nil? error))
      (is (= "2" result))))

  (testing "simple addition"
    (let [{:keys [result error]} (cli/process-request-text "2+2" nil)]
      (is (nil? error))
      (is (= "4" result))))

  (testing "addition with spaces"
    (let [{:keys [result error]} (cli/process-request-text "2 + 2" nil)]
      (is (nil? error))
      (is (= "4" result))))

  (testing "multiplication"
    (let [{:keys [result error]} (cli/process-request-text "3 * 4" nil)]
      (is (nil? error))
      (is (= "12" result))))

  (testing "parenthesized expression"
    (let [{:keys [result error]} (cli/process-request-text "3 * (4 + 5)" nil)]
      (is (nil? error))
      (is (= "27" result))))

  (testing "subtraction"
    (let [{:keys [result error]} (cli/process-request-text "10 - 3" nil)]
      (is (nil? error))
      (is (= "7" result))))

  (testing "division"
    (let [{:keys [result error]} (cli/process-request-text "10 / 2" nil)]
      (is (nil? error))
      (is (= "5" result))))

  (testing "decimal result"
    (let [{:keys [result error]} (cli/process-request-text "1 / 3" nil)]
      (is (nil? error))
      (is (some? result))))

  (testing "non-terminating decimal division does not throw"
    (let [{:keys [result error]} (cli/process-request-text "(2+2.5) / 77" nil)]
      (is (nil? error))
      (is (some? result))))

  (testing "decimal division with repeating result"
    (let [{:keys [result error]} (cli/process-request-text "10.0 / 3" nil)]
      (is (nil? error))
      (is (some? result)))))

(deftest math-does-not-break-unit-conversions
  (testing "unit conversion still works"
    (let [{:keys [result from target error]} (cli/process-request-text "12 feet in yards" nil)]
      (is (nil? error))
      (is (= "12 ft" from))
      (is (= "yd" target))
      (is (= "4" result))))

  (testing "temperature conversion still works"
    (let [{:keys [result error]} (cli/process-request-text "100 celsius in fahrenheit" nil)]
      (is (nil? error))
      (is (= "212" result)))))

(deftest math-with-format-opts
  (testing "precision applied to math result"
    (let [{:keys [result error]} (cli/process-request-text "1 / 3" {:round 2})]
      (is (nil? error))
      (is (= "0.33" result)))))

(deftest ratio-display
  (testing "ratios show fraction and approximation by default"
    (let [{:keys [result error]} (cli/process-request-text "21349 /234234" nil)]
      (is (nil? error))
      (is (re-find #"21349/234234 = " result))))

  (testing "ratios show only decimal with :numeric true"
    (let [{:keys [result error]} (cli/process-request-text "21349 /234234" {:numeric true})]
      (is (nil? error))
      (is (not (re-find #"=" result)))
      (is (re-find #"^0\.\d+" result))))

  (testing "reduced fraction shows original = reduced = decimal"
    (let [{:keys [result error]} (cli/process-request-text "2/10" nil)]
      (is (nil? error))
      (is (= "2/10 = 1/5 = 0.2" result))))

  (testing "already-reduced fraction shows fraction = decimal"
    (let [{:keys [result error]} (cli/process-request-text "1/5" nil)]
      (is (nil? error))
      (is (= "1/5 = 0.2" result))))

  (testing "reduced fraction with spaces shows original = reduced = decimal"
    (let [{:keys [result error]} (cli/process-request-text "4 / 8" nil)]
      (is (nil? error))
      (is (= "4 / 8 = 1/2 = 0.5" result)))))

(deftest display-uses-canonical-units
  (testing "long unit names become short forms"
    (let [{:keys [from target error]} (cli/process-request-text "12 feet in yards" nil)]
      (is (nil? error))
      (is (= "12 ft" from))
      (is (= "yd" target))))

  (testing "compound units use canonical short form"
    (let [{:keys [from error]} (cli/process-request-text "2 meters/second in kph" nil)]
      (is (nil? error))
      (is (= "2 m/s" from))))

  (testing "slash with trailing space normalizes to canonical"
    (let [{:keys [from error]} (cli/process-request-text "2 meters/ second in kph" nil)]
      (is (nil? error))
      (is (= "2 m/s" from))))

  (testing "slash with surrounding spaces normalizes to canonical"
    (let [{:keys [from error]} (cli/process-request-text "2 meters / second in kph" nil)]
      (is (nil? error))
      (is (= "2 m/s" from))))

  (testing "slash with excessive spaces normalizes to canonical"
    (let [{:keys [from error]} (cli/process-request-text "2 meters/   second in kph" nil)]
      (is (nil? error))
      (is (= "2 m/s" from))))

  (testing "target unit uses canonical form"
    (let [{:keys [target error]} (cli/process-request-text "2 m/s in kilometers per hour" nil)]
      (is (nil? error))
      (is (= "km/hr" target))))

  (testing "mixed quantities use canonical form"
    (let [{:keys [from error]} (cli/process-request-text "5 feet 11 inches in cm" nil)]
      (is (nil? error))
      (is (= "5 ft 11 in" from)))))

;; --- parse-format-opts tests ---

(deftest parse-format-opts-precision
  (testing "-p sets :round"
    (let [[opts remaining] (cli/parse-format-opts ["-p" "3" "12" "feet"])]
      (is (= 3 (:round opts)))
      (is (= ["12" "feet"] remaining))))

  (testing "--precision sets :round"
    (let [[opts remaining] (cli/parse-format-opts ["--precision" "5" "1" "inch"])]
      (is (= 5 (:round opts)))
      (is (= ["1" "inch"] remaining)))))

(deftest parse-format-opts-sig-figs
  (testing "-s sets :sig-figs"
    (let [[opts remaining] (cli/parse-format-opts ["-s" "4" "100" "meters"])]
      (is (= 4 (:sig-figs opts)))
      (is (= ["100" "meters"] remaining))))

  (testing "--sig-figs sets :sig-figs"
    (let [[opts remaining] (cli/parse-format-opts ["--sig-figs" "2" "5" "km"])]
      (is (= 2 (:sig-figs opts)))
      (is (= ["5" "km"] remaining)))))

(deftest parse-format-opts-conflict
  (testing "-p and -s together throws"
    (is (thrown-with-msg? Exception #"Cannot use both"
          (cli/parse-format-opts ["-p" "3" "-s" "4" "12" "feet"]))))

  (testing "-s then -p together throws"
    (is (thrown-with-msg? Exception #"Cannot use both"
          (cli/parse-format-opts ["-s" "2" "-p" "3" "12" "feet"])))))

(deftest parse-format-opts-no-flags
  (testing "no format flags returns nil opts"
    (let [[opts remaining] (cli/parse-format-opts ["12" "feet" "in" "yards"])]
      (is (nil? opts))
      (is (= ["12" "feet" "in" "yards"] remaining)))))

;; --- extract-flag tests ---

(deftest extract-flag-tests
  (testing "extracts short flag"
    (let [[value remaining] (cli/extract-flag ["-k" "length" "12" "feet"] "-k" "--kind")]
      (is (= "length" value))
      (is (= ["12" "feet"] remaining))))

  (testing "extracts long flag"
    (let [[value remaining] (cli/extract-flag ["--kind" "mass" "5" "kg"] "-k" "--kind")]
      (is (= "mass" value))
      (is (= ["5" "kg"] remaining))))

  (testing "returns nil when flag absent"
    (let [[value remaining] (cli/extract-flag ["12" "feet" "in" "yards"] "-k" "--kind")]
      (is (nil? value))
      (is (= ["12" "feet" "in" "yards"] remaining)))))

;; --- list-units tests ---

(deftest list-units-all
  (testing "list-units with no filter returns non-empty string"
    (let [output (cli/list-units nil)]
      (is (string? output))
      (is (pos? (count output)))))

  (testing "list-units includes length units"
    (let [output (cli/list-units nil)]
      (is (str/includes? output "meter"))
      (is (str/includes? output "foot")))))

(deftest list-units-by-kind
  (testing "list-units filtered to length"
    (let [output (cli/list-units "length")]
      (is (str/includes? output "meter"))
      (is (not (str/includes? output "kilogram")))))

  (testing "list-units filtered to mass"
    (let [output (cli/list-units "mass")]
      (is (str/includes? output "gram"))
      (is (not (str/includes? output "meter")))))

  (testing "list-units filtered to temperature"
    (let [output (cli/list-units "temperature")]
      (is (str/includes? output "ahrenheit"))
      (is (str/includes? output "elsius")))))

;; --- usage / help text ---

(deftest usage-text
  (testing "usage returns help string with expected sections"
    (let [output (cli/usage)]
      (is (str/includes? output "--list"))
      (is (str/includes? output "--kind"))
      (is (str/includes? output "--numeric"))
      (is (str/includes? output "--help")))))

;; --- numeric mode via process-request-text ---

(deftest numeric-mode
  (testing "numeric option returns bare number for conversion"
    (let [{:keys [result error]} (cli/process-request-text "12 feet in yards" {:numeric true})]
      (is (nil? error))
      (is (= "4" result))))

  (testing "numeric option suppresses ratio display"
    (let [{:keys [result error]} (cli/process-request-text "1/3" {:numeric true})]
      (is (nil? error))
      (is (not (str/includes? result "=")))
      (is (re-find #"^0\.\d+" result)))))

;; --- error output via process-request-text ---

(deftest error-cases
  (testing "unknown unit returns error"
    (let [{:keys [error]} (cli/process-request-text "12 floops in blargs" nil)]
      (is (some? error))))

  (testing "incompatible units return error"
    (let [{:keys [error]} (cli/process-request-text "12 feet in kilograms" nil)]
      (is (some? error)))))
