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

   {:group "Mixed quantities"
    :input "6 lb 4 oz in grams"
    :result "2834"
    :target "g"}

   {:group "Area and volume"
    :input "10 square feet in square meters"
    :result "0.9290304"
    :target "m"}

   {:group "Area and volume"
    :input "2 cubic yards to gallons"
    :result "403.948"
    :target "gal"}

   {:group "Area and volume"
    :input "100 sqft in sqm"
    :result "9.290304"
    :target "m"}

   {:group "Data units"
    :input "1 GB in MB"
    :result "1000"
    :target "MB"}

   {:group "Data units"
    :input "1 GiB in MiB"
    :result "1024"
    :target "MiB"}

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
    :target "lb"}

   {:group "Natural language"
    :input "how many cups are in a gallon?"
    :result "16"
    :target "cup"}

   {:group "Compound unit syntax"
    :input "60 mph in ft/s"
    :result "88"
    :target "ft/s"}

   {:group "Compound unit syntax"
    :input "100 sqft in sqm"
    :result "9.290304"
    :target "m"}])

(def math-cases
  [{:group "Arithmetic"
    :input "2 + 2"
    :result "4"}

   {:group "Arithmetic"
    :input "3 * (4 + 5)"
    :result "27"}

   {:group "Arithmetic"
    :input "10 - 3"
    :result "7"}

   {:group "Arithmetic"
    :input "2^10"
    :result "1024"}

   {:group "Roots"
    :input "sqrt(144)"
    :result "12"}

   {:group "Roots"
    :input "sqrt(25)"
    :result "5"}

   {:group "Roots"
    :input "2 * sqrt(25)"
    :result "10"}

   {:group "Roots"
    :input "sqrt(9) + sqrt(16)"
    :result "7"}

   {:group "Roots"
    :input "cube root of 27"
    :result "3"}

   {:group "Roots"
    :input "4th root of 625"
    :result "5"}

   {:group "Roots"
    :input "fifth root of 32"
    :result "2"}

   {:group "Roots"
    :input "square root of 2"
    :result "1.4142"}

   {:group "Roots"
    :input "root(3, 125)"
    :result "5"}

   {:group "Roots"
    :input "square root of 2.25"
    :result "1.5"}

   {:group "Roots"
    :input "square root of 1000000"
    :result "1000"}

   {:group "Roots"
    :input "cube root of 1000000"
    :result "100"}

   {:group "Root formatting"
    :input "square root of 2 rounded to 4 decimals"
    :result "1.4142"}

   {:group "Root formatting"
    :input "cube root of 2 with 5 sig figs"
    :result "1.2599"}

   {:group "Modulo"
    :input "10 mod 4"
    :result "2"}

   {:group "Modulo"
    :input "10 mod 3"
    :result "1"}

   {:group "Modulo"
    :input "15 modulo 4"
    :result "3"}

   {:group "Modulo"
    :input "9 mod 3"
    :result "0"}])

(def percentage-cases
  [{:group "Percentages"
    :input "15% of 50"
    :result "7.5"}

   {:group "Percentages"
    :input "50 percent of 200"
    :result "100"}

   {:group "Percentages"
    :input "10 is what percent of 100?"
    :result "10"}

   {:group "Percentages"
    :input "what percent of 100 is 25"
    :result "25"}

   {:group "Percentages"
    :input "what is 10 percent of 100"
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
    :error true}

   {:group "Errors"
    :input "12 feet in kilograms"
    :error true}])

(def all-cases
  (concat conversion-cases
          math-cases
          percentage-cases
          formatting-cases
          error-cases))
