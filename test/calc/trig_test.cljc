(ns calc.trig-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.test-helpers :as th]))

;; ==========================================================================
;; Parser tests — trig function phrases
;; ==========================================================================

(deftest parses-sin-shorthand
  (testing "'sin X' defaults to degrees"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "sin 0"
      {:op :trig :fn :sin :value 0 :angle-mode :deg}

      "sin 30"
      {:op :trig :fn :sin :value 30 :angle-mode :deg}

      "sin 45"
      {:op :trig :fn :sin :value 45 :angle-mode :deg}

      "sin 90"
      {:op :trig :fn :sin :value 90 :angle-mode :deg})))

(deftest parses-cos-shorthand
  (testing "'cos X' defaults to degrees"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "cos 0"
      {:op :trig :fn :cos :value 0 :angle-mode :deg}

      "cos 60"
      {:op :trig :fn :cos :value 60 :angle-mode :deg}

      "cos 90"
      {:op :trig :fn :cos :value 90 :angle-mode :deg})))

(deftest parses-tan-shorthand
  (testing "'tan X' defaults to degrees"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "tan 0"
      {:op :trig :fn :tan :value 0 :angle-mode :deg}

      "tan 45"
      {:op :trig :fn :tan :value 45 :angle-mode :deg})))

(deftest parses-trig-with-explicit-degrees
  (testing "'sin X degrees' / 'sin X deg'"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "sin 45 degrees"
      {:op :trig :fn :sin :value 45 :angle-mode :deg}

      "sin 45 deg"
      {:op :trig :fn :sin :value 45 :angle-mode :deg}

      "cos 60 degrees"
      {:op :trig :fn :cos :value 60 :angle-mode :deg}

      "tan 30 deg"
      {:op :trig :fn :tan :value 30 :angle-mode :deg})))

(deftest parses-trig-with-radians
  (testing "'sin X radians' / 'sin X rad'"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "sin 1 radians"
      {:op :trig :fn :sin :value 1 :angle-mode :rad}

      "sin 1 rad"
      {:op :trig :fn :sin :value 1 :angle-mode :rad}

      "cos 0 radians"
      {:op :trig :fn :cos :value 0 :angle-mode :rad}

      "tan 0.5 rad"
      {:op :trig :fn :tan :value 0.5 :angle-mode :rad})))

(deftest parses-trig-with-pi
  (testing "'sin pi' parses pi as a value in radians"
    (is (th/deep== {:op :trig :fn :sin :value Math/PI :angle-mode :rad}
           (parser/parse-request "sin pi"))))

  (testing "'cos pi/4' parses pi division"
    (let [parsed (parser/parse-request "cos pi/4")]
      (is (= :trig (:op parsed)))
      (is (= :cos (:fn parsed)))
      (is (= :rad (:angle-mode parsed)))
      (is (th/approx== (/ Math/PI 4) (:value parsed)))))

  (testing "'sin pi/2 radians' — explicit radians with pi"
    (let [parsed (parser/parse-request "sin pi/2 radians")]
      (is (= :trig (:op parsed)))
      (is (= :sin (:fn parsed)))
      (is (= :rad (:angle-mode parsed)))
      (is (th/approx== (/ Math/PI 2) (:value parsed))))))

;; ==========================================================================
;; Parser tests — inverse trig
;; ==========================================================================

(deftest parses-asin-shorthand
  (testing "'asin X' and 'arcsin X' default to degree output"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "asin 0.5"
      {:op :trig :fn :asin :value 0.5 :angle-mode :deg}

      "arcsin 0.5"
      {:op :trig :fn :asin :value 0.5 :angle-mode :deg}

      "asin 1"
      {:op :trig :fn :asin :value 1 :angle-mode :deg})))

(deftest parses-acos-shorthand
  (testing "'acos X' and 'arccos X'"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "acos 0.5"
      {:op :trig :fn :acos :value 0.5 :angle-mode :deg}

      "arccos 0"
      {:op :trig :fn :acos :value 0 :angle-mode :deg})))

(deftest parses-atan-shorthand
  (testing "'atan X' and 'arctan X'"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "atan 1"
      {:op :trig :fn :atan :value 1 :angle-mode :deg}

      "arctan 0"
      {:op :trig :fn :atan :value 0 :angle-mode :deg})))

(deftest parses-inverse-trig-in-radians
  (testing "'asin X in radians' overrides output to radians"
    (are [phrase expected] (th/deep== expected (parser/parse-request phrase))
      "asin 0.5 in radians"
      {:op :trig :fn :asin :value 0.5 :angle-mode :rad}

      "acos 0.5 in rad"
      {:op :trig :fn :acos :value 0.5 :angle-mode :rad}

      "arctan 1 in radians"
      {:op :trig :fn :atan :value 1 :angle-mode :rad})))

;; ==========================================================================
;; Parser tests — degrees/radians conversion (unit system)
;; ==========================================================================

(deftest parses-degree-radian-conversion
  (testing "'45 degrees in radians' uses standard unit conversion"
    (let [parsed (parser/parse-request "45 degrees in radians")]
      (is (= :convert (:op parsed)))
      (is (th/deep== {:value 45 :unit :deg} (:quantity parsed)))
      (is (= :rad (:to parsed)))))

  (testing "'1.5708 radians in degrees'"
    (let [parsed (parser/parse-request "1.5708 radians in degrees")]
      (is (= :convert (:op parsed)))
      (is (th/deep== {:value 1.5708 :unit :rad} (:quantity parsed)))
      (is (= :deg (:to parsed)))))

  (testing "short forms: 'pi rad to deg'"
    (let [parsed (parser/parse-request "pi rad to deg")]
      (is (= :convert (:op parsed)))
      (is (= :rad (:unit (:quantity parsed))))
      (is (= :deg (:to parsed)))
      (is (th/approx== Math/PI (:value (:quantity parsed)))))))

;; ==========================================================================
;; Parser tests — trig with formatting
;; ==========================================================================

(deftest parses-trig-with-formatting
  (testing "trig with 'rounded to N decimals'"
    (is (th/deep== {:op :trig :fn :sin :value 45 :angle-mode :deg
                    :format {:round 4}}
           (parser/parse-request "sin 45 degrees rounded to 4 decimals"))))

  (testing "trig with 'with N sig figs'"
    (is (th/deep== {:op :trig :fn :cos :value 30 :angle-mode :deg
                    :format {:sig-figs 6}}
           (parser/parse-request "cos 30 deg with 6 sig figs")))))

;; ==========================================================================
;; Eval tests — trig evaluation
;; ==========================================================================

(deftest evaluates-sin
  (testing "sin of common degree values"
    (are [deg expected] (let [r (ev/convert-request {:op :trig :fn :sin :value deg :angle-mode :deg})]
                          (and (:ok? r) (th/approx== expected (:value r))))
      0    0.0
      30   0.5
      45   0.7071067811865476
      60   0.8660254037844386
      90   1.0
      180  0.0
      270  -1.0
      360  0.0))

  (testing "sin in radians"
    (let [r (ev/convert-request {:op :trig :fn :sin :value (/ Math/PI 6) :angle-mode :rad})]
      (is (:ok? r))
      (is (th/approx== 0.5 (:value r))))))

(deftest evaluates-cos
  (testing "cos of common degree values"
    (are [deg expected] (let [r (ev/convert-request {:op :trig :fn :cos :value deg :angle-mode :deg})]
                          (and (:ok? r) (th/approx== expected (:value r))))
      0    1.0
      60   0.5
      90   0.0
      180  -1.0
      360  1.0)))

(deftest evaluates-tan
  (testing "tan of common degree values"
    (are [deg expected] (let [r (ev/convert-request {:op :trig :fn :tan :value deg :angle-mode :deg})]
                          (and (:ok? r) (th/approx== expected (:value r))))
      0    0.0
      45   1.0
      60   1.7320508075688772
      180  0.0)))

(deftest evaluates-asin
  (testing "asin returns degrees by default"
    (are [x expected-deg] (let [r (ev/convert-request {:op :trig :fn :asin :value x :angle-mode :deg})]
                            (and (:ok? r) (th/approx== expected-deg (:value r))))
      0    0.0
      0.5  30.0
      1    90.0
      -1   -90.0))

  (testing "asin in radians"
    (let [r (ev/convert-request {:op :trig :fn :asin :value 0.5 :angle-mode :rad})]
      (is (:ok? r))
      (is (th/approx== (/ Math/PI 6) (:value r))))))

(deftest evaluates-acos
  (testing "acos returns degrees by default"
    (are [x expected-deg] (let [r (ev/convert-request {:op :trig :fn :acos :value x :angle-mode :deg})]
                            (and (:ok? r) (th/approx== expected-deg (:value r))))
      1    0.0
      0.5  60.0
      0    90.0
      -1   180.0)))

(deftest evaluates-atan
  (testing "atan returns degrees by default"
    (are [x expected-deg] (let [r (ev/convert-request {:op :trig :fn :atan :value x :angle-mode :deg})]
                            (and (:ok? r) (th/approx== expected-deg (:value r))))
      0    0.0
      1    45.0
      -1   -45.0)))

;; ==========================================================================
;; Eval tests — degree/radian unit conversion
;; ==========================================================================

(deftest evaluates-degree-radian-conversion
  (testing "degrees to radians"
    (let [r (ev/convert-request {:op :convert :quantity {:value 180 :unit :deg} :to :rad})]
      (is (:ok? r))
      (is (th/approx== Math/PI (:value r)))))

  (testing "radians to degrees"
    (let [r (ev/convert-request {:op :convert :quantity {:value Math/PI :unit :rad} :to :deg})]
      (is (:ok? r))
      (is (th/approx== 180.0 (:value r)))))

  (testing "90 degrees to radians"
    (let [r (ev/convert-request {:op :convert :quantity {:value 90 :unit :deg} :to :rad})]
      (is (:ok? r))
      (is (th/approx== (/ Math/PI 2) (:value r)))))

  (testing "full circle"
    (let [r (ev/convert-request {:op :convert :quantity {:value 360 :unit :deg} :to :rad})]
      (is (:ok? r))
      (is (th/approx== (* 2 Math/PI) (:value r))))))

;; ==========================================================================
;; Eval tests — trig result unit labels
;; ==========================================================================

(deftest trig-result-has-unit-label
  (testing "inverse trig in degrees has degree symbol"
    (let [r (ev/convert-request {:op :trig :fn :asin :value 0.5 :angle-mode :deg})]
      (is (:ok? r))
      (is (= "°" (:unit-label r)))))

  (testing "inverse trig in radians has rad label"
    (let [r (ev/convert-request {:op :trig :fn :asin :value 0.5 :angle-mode :rad})]
      (is (:ok? r))
      (is (= "rad" (:unit-label r)))))

  (testing "forward trig has no unit label"
    (let [r (ev/convert-request {:op :trig :fn :sin :value 45 :angle-mode :deg})]
      (is (:ok? r))
      (is (nil? (:unit-label r))))))

;; ==========================================================================
;; End-to-end tests
;; ==========================================================================

(deftest e2e-sin
  (testing "sin 30"
    (let [{:keys [result]} (th/evaluate "sin 30" nil)]
      (is (= "sin 30° = 0.5" result))))

  (testing "sin 90"
    (let [{:keys [result]} (th/evaluate "sin 90" nil)]
      (is (= "sin 90° = 1" result))))

  (testing "sin 0"
    (let [{:keys [result]} (th/evaluate "sin 0" nil)]
      (is (= "sin 0° = 0" result))))

  (testing "sin 45 rounded to 4 decimals"
    (let [{:keys [result]} (th/evaluate "sin 45 degrees rounded to 4 decimals" nil)]
      (is (= "sin 45° = 0.7071" result)))))

(deftest e2e-cos
  (testing "cos 60"
    (let [{:keys [result]} (th/evaluate "cos 60" nil)]
      (is (= "cos 60° = 0.5" result))))

  (testing "cos 0"
    (let [{:keys [result]} (th/evaluate "cos 0" nil)]
      (is (= "cos 0° = 1" result)))))

(deftest e2e-tan
  (testing "tan 45"
    (let [{:keys [result]} (th/evaluate "tan 45" nil)]
      (is (= "tan 45° = 1" result)))))

(deftest e2e-inverse-trig
  (testing "asin 0.5 returns 30 degrees"
    (let [{:keys [result]} (th/evaluate "asin 0.5" nil)]
      (is (= "asin 0.5 = 30°" result))))

  (testing "arccos 0.5 returns 60 degrees"
    (let [{:keys [result]} (th/evaluate "arccos 0.5" nil)]
      (is (= "acos 0.5 = 60°" result))))

  (testing "atan 1 returns 45 degrees"
    (let [{:keys [result]} (th/evaluate "atan 1" nil)]
      (is (= "atan 1 = 45°" result))))

  (testing "asin 0.5 in radians"
    (let [{:keys [result]} (th/evaluate "asin 0.5 in radians" nil)]
      (is (str/includes? result "asin 0.5"))
      (is (str/ends-with? result "rad")))))

(deftest e2e-degree-radian-conversion
  (testing "180 degrees in radians"
    (let [{:keys [result]} (th/evaluate "180 degrees in radians" nil)]
      (is (some? result))
      (is (str/starts-with? result "3.14"))))

  (testing "45 deg to rad"
    (let [{:keys [result]} (th/evaluate "45 deg to rad" nil)]
      (is (some? result))
      (is (str/starts-with? result "0.785")))))

(deftest e2e-trig-with-pi
  (testing "sin pi"
    (let [{:keys [result]} (th/evaluate "sin pi" nil)]
      (is (some? result))
      (is (str/includes? result "rad"))
      (is (str/includes? result "= 0"))))

  (testing "cos pi/4"
    (let [{:keys [result]} (th/evaluate "cos pi/4" nil)]
      (is (some? result))
      (is (str/includes? result "rad"))
      (is (str/includes? result "0.707")))))

;; ==========================================================================
;; Math expression composition tests
;; ==========================================================================

(deftest trig-in-math-expressions
  (testing "standalone trig goes through parse-trig, not parse-math"
    (is (nil? (parser/parse-math "sin(30)"))
        "standalone trig should not match parse-math"))

  (testing "compound trig returns annotated map"
    (let [r (parser/parse-math "cos 32 / sin 45")]
      (is (map? r))
      (is (= "cos 32° / sin 45°" (:trig-expr r))))
    (let [r (parser/parse-math "cos 32 rad / sin 45 rad")]
      (is (map? r))
      (is (= "cos 32 rad / sin 45 rad" (:trig-expr r)))))

  (testing "trig composed with arithmetic"
    (are [expr expected] (th/approx== expected (parser/math-value (parser/parse-math expr)))
      "sin(45) + cos(60)"    1.2071067811865476
      "sin(30) * 2"          1.0
      "2 * cos(60)"          1.0
      "sin(90) - cos(0)"     0.0
      "tan(45) + 1"          2.0
      "sin(30) ^ 2"          0.25))

  (testing "trig composed with trig"
    (are [expr expected] (th/approx== expected (parser/math-value (parser/parse-math expr)))
      "sin(30) + cos(60)"    1.0
      "sin(45) * sin(45)"    0.5
      "asin(sin(30))"        30.0)))

(deftest trig-bare-form-in-compound-expressions
  (testing "bare trig with operators (no parens)"
    (let [r (parser/math-value (parser/parse-math "cos 32 / sin 45"))]
      (is (some? r))
      (is (th/approx== (/ (Math/cos (* 32 (/ Math/PI 180)))
                          (Math/sin (* 45 (/ Math/PI 180))))
                       r)))

    (let [r (parser/math-value (parser/parse-math "sin 30 + cos 60"))]
      (is (some? r))
      (is (th/approx== 1.0 r)))

    (let [r (parser/math-value (parser/parse-math "tan 45 * 2"))]
      (is (some? r))
      (is (th/approx== 2.0 r)))))

(deftest trig-with-angle-mode-in-math
  (testing "rad/deg suffix in compound math expressions"
    (let [r (parser/math-value (parser/parse-math "cos 32 rad / sin 45 rad"))]
      (is (some? r))
      (is (th/approx== (/ (Math/cos 32) (Math/sin 45)) r)))

    (let [r (parser/math-value (parser/parse-math "sin(1 rad) + cos(0 rad)"))]
      (is (some? r))
      (is (th/approx== (+ (Math/sin 1) (Math/cos 0)) r)))

    (let [r (parser/math-value (parser/parse-math "sin 30 deg + cos 60 deg"))]
      (is (some? r))
      (is (th/approx== 1.0 r)))))

(deftest e2e-trig-compound-expressions
  (testing "cos 32 / sin 45 shows degree annotation"
    (let [{:keys [result]} (th/evaluate "cos 32 / sin 45" nil)]
      (is (str/includes? result "cos 32°"))
      (is (str/includes? result "sin 45°"))
      (is (str/includes? result "1.19"))))

  (testing "cos 32 rad / sin 45 rad preserves rad annotation"
    (let [{:keys [result]} (th/evaluate "cos 32 rad / sin 45 rad" nil)]
      (is (str/includes? result "cos 32 rad"))
      (is (str/includes? result "sin 45 rad"))
      (is (str/includes? result "0.980"))))

  (testing "sin(45) + cos(60) shows degree annotation"
    (let [{:keys [result]} (th/evaluate "sin(45) + cos(60)" nil)]
      (is (str/includes? result "sin(45°)"))
      (is (str/includes? result "cos(60°)"))
      (is (str/includes? result "1.207"))))

  (testing "sin 30 + cos 60"
    (let [{:keys [result]} (th/evaluate "sin 30 + cos 60" nil)]
      (is (str/includes? result "sin 30°"))
      (is (str/includes? result "= 1"))))

  (testing "2 * sin(30)"
    (let [{:keys [result]} (th/evaluate "2 * sin(30)" nil)]
      (is (str/includes? result "sin(30°)"))
      (is (str/includes? result "= 1")))))

;; ==========================================================================
;; Standalone pi / π constant tests
;; ==========================================================================

(deftest parse-standalone-pi
  (testing "bare 'pi' parses as math-expr"
    (let [parsed (parser/parse-request "pi")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== Math/PI (:value parsed)))))

  (testing "bare 'π' (unicode) parses as math-expr"
    (let [parsed (parser/parse-request "π")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== Math/PI (:value parsed)))))

  (testing "'pi/4' parses as math-expr"
    (let [parsed (parser/parse-request "pi/4")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== (/ Math/PI 4) (:value parsed)))))

  (testing "'2*pi' parses as math-expr"
    (let [parsed (parser/parse-request "2*pi")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== (* 2 Math/PI) (:value parsed)))))

  (testing "'pi*2' parses as math-expr"
    (let [parsed (parser/parse-request "pi*2")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== (* 2 Math/PI) (:value parsed))))))

(deftest e2e-standalone-pi
  (testing "pi returns its value"
    (let [{:keys [result]} (th/evaluate "pi" nil)]
      (is (some? result))
      (is (str/starts-with? result "3.14"))))

  (testing "π (unicode) returns its value"
    (let [{:keys [result]} (th/evaluate "π" nil)]
      (is (some? result))
      (is (str/starts-with? result "3.14"))))

  (testing "pi/4 returns correct value"
    (let [{:keys [result]} (th/evaluate "pi/4" nil)]
      (is (some? result))
      (is (str/starts-with? result "0.785"))))

  (testing "2*pi returns correct value"
    (let [{:keys [result]} (th/evaluate "2*pi" nil)]
      (is (some? result))
      (is (str/starts-with? result "6.28"))))

  (testing "pi+1 returns correct value"
    (let [{:keys [result]} (th/evaluate "pi+1" nil)]
      (is (some? result))
      (is (str/starts-with? result "4.14"))))

  (testing "pi^2 returns correct value"
    (let [{:keys [result]} (th/evaluate "pi^2" nil)]
      (is (some? result))
      (is (str/starts-with? result "9.86"))))

  (testing "pi still works inside trig: sin pi"
    (let [{:keys [result]} (th/evaluate "sin pi" nil)]
      (is (some? result))
      (is (str/includes? result "rad"))
      (is (str/includes? result "= 0"))))

  (testing "pi still works inside trig: cos pi/4"
    (let [{:keys [result]} (th/evaluate "cos pi/4" nil)]
      (is (some? result))
      (is (str/includes? result "rad"))
      (is (str/includes? result "0.707"))))

  (testing "π inside trig: sin π"
    (let [{:keys [result]} (th/evaluate "sin π" nil)]
      (is (some? result))
      (is (str/includes? result "= 0")))))

;; ==========================================================================
;; Standalone e (Euler's number) and phi (golden ratio) constant tests
;; ==========================================================================

(deftest parse-standalone-e
  (testing "bare 'e' parses as math-expr"
    (let [parsed (parser/parse-request "e")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== Math/E (:value parsed)))))

  (testing "'e^2' parses as math-expr"
    (let [parsed (parser/parse-request "e^2")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== (* Math/E Math/E) (:value parsed)))))

  (testing "'2*e' parses as math-expr"
    (let [parsed (parser/parse-request "2*e")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== (* 2 Math/E) (:value parsed))))))

(deftest parse-standalone-phi
  (testing "bare 'phi' parses as math-expr"
    (let [parsed (parser/parse-request "phi")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== 1.6180339887498948 (:value parsed)))))

  (testing "bare 'φ' (unicode) parses as math-expr"
    (let [parsed (parser/parse-request "φ")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== 1.6180339887498948 (:value parsed)))))

  (testing "'phi^2' parses as math-expr"
    (let [parsed (parser/parse-request "phi^2")]
      (is (= :math-expr (:op parsed)))
      (is (th/approx== (* 1.6180339887498948 1.6180339887498948) (:value parsed))))))

(deftest e2e-standalone-e
  (testing "e returns its value"
    (let [{:keys [result]} (th/evaluate "e" nil)]
      (is (some? result))
      (is (str/starts-with? result "2.718"))))

  (testing "e^2 returns correct value"
    (let [{:keys [result]} (th/evaluate "e^2" nil)]
      (is (some? result))
      (is (str/starts-with? result "7.389"))))

  (testing "e*pi returns correct value"
    (let [{:keys [result]} (th/evaluate "e*pi" nil)]
      (is (some? result))
      (is (str/starts-with? result "8.539")))))

(deftest e2e-standalone-phi
  (testing "phi returns its value"
    (let [{:keys [result]} (th/evaluate "phi" nil)]
      (is (some? result))
      (is (str/starts-with? result "1.618"))))

  (testing "φ (unicode) returns its value"
    (let [{:keys [result]} (th/evaluate "φ" nil)]
      (is (some? result))
      (is (str/starts-with? result "1.618"))))

  (testing "phi^2 = phi + 1 (golden ratio property)"
    (let [{:keys [result]} (th/evaluate "phi^2" nil)]
      (is (some? result))
      (is (str/starts-with? result "2.618"))))

  (testing "unit conversions still work (no e/phi interference)"
    (let [{:keys [result]} (th/evaluate "10 meters in feet" nil)]
      (is (some? result))
      (is (str/starts-with? result "32.8")))))
