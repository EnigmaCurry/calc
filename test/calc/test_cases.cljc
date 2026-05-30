(ns calc.test-cases)

;; Shared test cases for all environments (Babashka, JVM Clojure, ClojureScript).
;;
;; Each entry is a map with:
;;   :input    — the natural language string to evaluate
;;   :result   — expected :result string (substring match)
;;   :target   — expected :target string, or nil if none expected
;;   :error    — if truthy, expect an error instead of a result
;;   :group    — category label for reporting
;;
;; Tests use substring matching on :result so that minor formatting
;; differences between JVM (exact rationals) and JS (floats) don't
;; cause false failures.

(def conversion-cases
  [{:group "Basic conversions"
    :input "12 feet in yards"
    :result "4"
    :target "yd"}

   {:group "Basic conversions"
    :input "5 miles to km"
    :result "8.04672"
    :target "km"}

   {:group "Basic conversions"
    :input "3 gallons in liters"
    :result "11.356"
    :target "L"}

   {:group "Basic conversions"
    :input "100 pounds to kilograms"
    :result "45.359"
    :target "kg"}

   {:group "Temperature"
    :input "212 fahrenheit to celsius"
    :result "100"
    :target "°C"}

   {:group "Temperature"
    :input "32 F in C"
    :result "0"
    :target "°C"}

   {:group "Temperature"
    :input "100 celsius to fahrenheit"
    :result "212"
    :target "°F"}

   {:group "Compound units"
    :input "60 mph in ft/s"
    :result "88"
    :target "ft/s"}

   {:group "Compound units"
    :input "100 MB / 10 Mbps in seconds"
    :result "80"
    :target "s"}

   {:group "Mixed quantities"
    :input "5 feet 11 inches to cm"
    :result "180.34"
    :target "cm"}

   {:group "Mixed quantities"
    :input "1 hour 30 minutes in minutes"
    :result "90"
    :target "min"}

   {:group "Area and volume"
    :input "10 square feet in square meters"
    :result "0.9290304"
    :target "m"}

   {:group "Area and volume"
    :input "2 cubic yards to gallons"
    :result "403.948"
    :target "gal"}

   {:group "Data units"
    :input "1 GB in MB"
    :result "1000"
    :target "MB"}

   {:group "Natural language"
    :input "how many inches are in 3 feet?"
    :result "36"
    :target "in"}

   {:group "Natural language"
    :input "convert 12 feet to yards"
    :result "4"
    :target "yd"}

   {:group "Natural language"
    :input "what is 5 kg in pounds?"
    :result "11.023"
    :target "lb"}])

(def math-cases
  [{:group "Arithmetic"
    :input "2 + 2"
    :result "4"}

   {:group "Arithmetic"
    :input "3 * (4 + 5)"
    :result "27"}

   {:group "Roots"
    :input "sqrt(144)"
    :result "12"}

   {:group "Roots"
    :input "cube root of 27"
    :result "3"}

   {:group "Roots"
    :input "4th root of 625"
    :result "5"}

   {:group "Roots"
    :input "square root of 2"
    :result "1.4142"}])

(def percentage-cases
  [{:group "Percentages"
    :input "15% of 50"
    :result "7.5"}

   {:group "Percentages"
    :input "50 percent of 200"
    :result "100"}

   {:group "Percentages"
    :input "10 is what percent of 100?"
    :result "10"}])

(def formatting-cases
  [{:group "Formatting"
    :input "12 feet in yards rounded to 2 decimals"
    :result "4.00"
    :target "yd"}

   {:group "Formatting"
    :input "5 miles in km with 3 sig figs"
    :result "8.05"
    :target "km"}

   {:group "Formatting"
    :input "7 inches in feet as a fraction"
    :result "7/12"
    :target "ft"}])

(def error-cases
  [{:group "Errors"
    :input "flurble glorb"
    :error true}

   {:group "Errors"
    :input "12 blorps in meters"
    :error true}])

(def all-cases
  (concat conversion-cases
          math-cases
          percentage-cases
          formatting-cases
          error-cases))
