(ns unitz.parser-test
  (:require [clojure.test :refer [deftest testing is are]]
            [unitz.parser :as parser]))

(deftest parses-simple-scalar-conversions
  (testing "basic '<number> <unit> in/to <unit>' phrases"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 ft in yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "5 miles to km"
      {:op :convert
       :quantity {:value 5N :unit :mi}
       :to :km}

      "3 gallons in liters"
      {:op :convert
       :quantity {:value 3N :unit :gal}
       :to :l}

      "100 pounds to kilograms"
      {:op :convert
       :quantity {:value 100N :unit :lb}
       :to :kg})))

(deftest parses-unit-aliases-and-plurals
  (testing "aliases normalize before reaching the conversion engine"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 feet in yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "1 foot in inches"
      {:op :convert
       :quantity {:value 1N :unit :ft}
       :to :in}

      "2 meters to feet"
      {:op :convert
       :quantity {:value 2N :unit :m}
       :to :ft}

      "16 ounces in pounds"
      {:op :convert
       :quantity {:value 16N :unit :oz}
       :to :lb})))

(deftest parses-compact-input
  (testing "number and unit may be adjacent"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12ft in yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "5km to miles"
      {:op :convert
       :quantity {:value 5N :unit :km}
       :to :mi}

      "100kg in lb"
      {:op :convert
       :quantity {:value 100N :unit :kg}
       :to :lb})))

(deftest parses-natural-language-question-forms
  (testing "common filler words do not affect the request"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "how many yards is 12 feet?"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "what is 12 ft in yards?"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "convert 12 feet to yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "how much is 5 kg in pounds?"
      {:op :convert
       :quantity {:value 5N :unit :kg}
       :to :lb})))

(deftest parses-reversed-how-many-form
  (testing "'how many target units are in value source unit' reverses target/source correctly"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "how many inches are in 3 feet?"
      {:op :convert
       :quantity {:value 3N :unit :ft}
       :to :in}

      "how many cups are in a gallon?"
      {:op :convert
       :quantity {:value 1N :unit :gal}
       :to :cup}

      "how many teaspoons are in 2 tablespoons?"
      {:op :convert
       :quantity {:value 2N :unit :tbsp}
       :to :tsp})))

(deftest parses-x-is-how-many-y-form
  (testing "'source quantity is how many target units' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 feet is how many yards?"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "100 kg is how many pounds?"
      {:op :convert
       :quantity {:value 100N :unit :kg}
       :to :lb})))

(deftest parses-temperature-conversions
  (testing "temperature still parses like scalar conversion; evaluator handles affine math"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "32 F in C"
      {:op :convert
       :quantity {:value 32N :unit :degF}
       :to :degC}

      "100 celsius to fahrenheit"
      {:op :convert
       :quantity {:value 100N :unit :degC}
       :to :degF}

      "273.15 K in C"
      {:op :convert
       :quantity {:value 273.15M :unit :K}
       :to :degC})))

(deftest parses-compound-rate-units
  (testing "compound units become exponent maps"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "60 miles per hour in km/h"
      {:op :convert
       :quantity {:value 60N :unit {:mi 1 :hr -1}}
       :to {:km 1 :hr -1}}

      "10 meters per second to mph"
      {:op :convert
       :quantity {:value 10N :unit {:m 1 :s -1}}
       :to {:mi 1 :hr -1}}

      "5 ft/s in m/s"
      {:op :convert
       :quantity {:value 5N :unit {:ft 1 :s -1}}
       :to {:m 1 :s -1}})))

(deftest parses-area-and-volume-units
  (testing "square/cubic modifiers become powers"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "10 square feet in square meters"
      {:op :convert
       :quantity {:value 10N :unit {:ft 2}}
       :to {:m 2}}

      "2 cubic yards in gallons"
      {:op :convert
       :quantity {:value 2N :unit {:yd 3}}
       :to :gal}

      "1 acre in square feet"
      {:op :convert
       :quantity {:value 1N :unit :acre}
       :to {:ft 2}})))

(deftest parses-derived-named-units
  (testing "named derived units may appear beside structural compound units"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1 newton in kg m/s^2"
      {:op :convert
       :quantity {:value 1N :unit :N}
       :to {:kg 1 :m 1 :s -2}}

      "1 joule in newton meters"
      {:op :convert
       :quantity {:value 1N :unit :J}
       :to {:N 1 :m 1}}

      "1 watt in joules per second"
      {:op :convert
       :quantity {:value 1N :unit :W}
       :to {:J 1 :s -1}}

      "1 psi in pascals"
      {:op :convert
       :quantity {:value 1N :unit :psi}
       :to :Pa})))

(deftest parses-data-units
  (testing "data units preserve decimal/binary distinction"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1024 bytes in kilobytes"
      {:op :convert
       :quantity {:value 1024N :unit :B}
       :to :KB}

      "1 GiB in MiB"
      {:op :convert
       :quantity {:value 1N :unit :GiB}
       :to :MiB}

      "1 megabit in megabytes"
      {:op :convert
       :quantity {:value 1N :unit :Mb}
       :to :MB}

      "100 Mbps in MB/s"
      {:op :convert
       :quantity {:value 100N :unit {:Mb 1 :s -1}}
       :to {:MB 1 :s -1}})))

(deftest parses-mixed-quantities
  (testing "multiple compatible quantity terms become a vector"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1 hour 30 minutes in minutes"
      {:op :convert
       :quantity [{:value 1N :unit :hr}
                  {:value 30N :unit :min}]
       :to :min}

      "2 days 4 hours in hours"
      {:op :convert
       :quantity [{:value 2N :unit :day}
                  {:value 4N :unit :hr}]
       :to :hr}

      "5 feet 11 inches in cm"
      {:op :convert
       :quantity [{:value 5N :unit :ft}
                  {:value 11N :unit :in}]
       :to :cm}

      "6 lb 4 oz in grams"
      {:op :convert
       :quantity [{:value 6N :unit :lb}
                  {:value 4N :unit :oz}]
       :to :g})))

(deftest parses-fractions
  (testing "fractions remain exact ratios when possible"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1/2 cup in tablespoons"
      {:op :convert
       :quantity {:value 1/2 :unit :cup}
       :to :tbsp}

      "3 1/2 inches in cm"
      {:op :convert
       :quantity {:value 7/2 :unit :in}
       :to :cm}

      "half a gallon in liters"
      {:op :convert
       :quantity {:value 1/2 :unit :gal}
       :to :l}

      "quarter mile in meters"
      {:op :convert
       :quantity {:value 1/4 :unit :mi}
       :to :m})))

(deftest parses-approximate-requests
  (testing "approximation intent is preserved for formatting"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "about 12 feet in meters"
      {:op :convert
       :approx? true
       :quantity {:value 12N :unit :ft}
       :to :m}

      "roughly 5 miles in km"
      {:op :convert
       :approx? true
       :quantity {:value 5N :unit :mi}
       :to :km}

      "~3 cups in ml"
      {:op :convert
       :approx? true
       :quantity {:value 3N :unit :cup}
       :to :ml})))

(deftest parses-formatting-requests
  (testing "formatting is separate from conversion semantics"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 ft in yards rounded to 2 decimals"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd
       :format {:round 2}}

      "5 miles in km with 3 sig figs"
      {:op :convert
       :quantity {:value 5N :unit :mi}
       :to :km
       :format {:sig-figs 3}}

      "1/3 meter in inches as a fraction"
      {:op :convert
       :quantity {:value 1/3 :unit :m}
       :to :in
       :format {:style :fraction}})))

(deftest parser-reports-unknown-units
  (testing "unknown units should produce data, not throw weird exceptions"
    (is (= {:error :unknown-unit
            :unit "blorps"
            :phrase "12 blorps in meters"}
           (parser/parse-request "12 blorps in meters")))))

(deftest parser-reports-unparseable-phrases
  (testing "nonsense input gets a useful parse error"
    (is (= {:error :unparseable
            :phrase "banana canoe surprise"}
           (parser/parse-request "banana canoe surprise")))))
