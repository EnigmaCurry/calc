(ns calc.parser
  (:require [clojure.string :as str]
            [calc.units :as units]
            [calc.math :as m]
            [calc.dice :as dice])
  #?(:lpy (:import math)))

(def ^:private pi-value
  #?(:clj  (bigdec Math/PI)
     :cljs (m/dec->double (m/->dec m/PI))
     :lpy  (bigdec m/PI)))

(def ^:private e-value
  #?(:clj  (bigdec Math/E)
     :cljs (m/dec->double (m/->dec m/E))
     :lpy  (bigdec m/E)))

(def ^:private phi-value
  "The golden ratio φ = (1 + √5) / 2"
  #?(:clj  (bigdec 1.6180339887498948482)
     :cljs (m/dec->double (m/ddiv (m/d+ 1 (m/dsqrt 5)) 2))
     :lpy  (bigdec 1.6180339887498948482)))

(def ^:private math-constants
  {"pi" pi-value "π" pi-value
   "e" e-value
   "phi" phi-value "φ" phi-value})

(def unit-aliases units/unit-aliases)

(def special-unit-forms
  {"mph" {:mi 1 :hr -1}
   "kph" {:km 1 :hr -1}
   "km/h" {:km 1 :hr -1}
   "kmph" {:km 1 :hr -1}
   "mps" {:m 1 :s -1}
   "m/s" {:m 1 :s -1}
   "fps" {:ft 1 :s -1}
   "ft/s" {:ft 1 :s -1}

   ;; Area / volume shorthand
   "sqft" {:ft 2}
   "sqm"  {:m 2}
   "sqyd" {:yd 2}
   "sqmi" {:mi 2}
   "sqkm" {:km 2}
   "sqin" {:in 2}
   "cuft" {:ft 3}
   "cum"  {:m 3}
   "cuyd" {:yd 3}

   ;; Bit rates (lowercase b = bits)
   "bps"  {:bit 1 :s -1}
   "Kbps" {:Kb 1 :s -1}
   "kbps" {:Kb 1 :s -1}
   "Mbps" {:Mb 1 :s -1}
   "mbps" {:Mb 1 :s -1}
   "Gbps" {:Gb 1 :s -1}
   "gbps" {:Gb 1 :s -1}
   "Tbps" {:Tb 1 :s -1}
   "tbps" {:Tb 1 :s -1}
   "Pbps" {:Pb 1 :s -1}
   "pbps" {:Pb 1 :s -1}
   "Ebps" {:Eb 1 :s -1}
   "ebps" {:Eb 1 :s -1}

   ;; Byte rates (uppercase B = bytes)
   "KBps" {:KB 1 :s -1}
   "MBps" {:MB 1 :s -1}
   "GBps" {:GB 1 :s -1}
   "TBps" {:TB 1 :s -1}
   "PBps" {:PB 1 :s -1}
   "EBps" {:EB 1 :s -1}})

(defn clean-phrase [s]
  (-> s
      str/trim
      (str/replace #"[?]" "")
      (str/replace #"," "")
      (str/replace #"~\s*" "~ ")
      ;; Expand abutted "in" to "inch" so standalone "in" is always a connector.
      ;; 12in → 12 inch, 3.5in → 3.5 inch (but 12inch, 12inches unchanged)
      (str/replace #"(?i)(\d)in\b" "$1 inch")
      ;; 12ft -> 12 ft, 100kg -> 100 kg
      ;; But preserve ordinals like 4th, 2nd, 3rd, 5th etc.
      ;; And preserve scientific notation like 10E9, 3.5e-12
      (str/replace #"(\d)(?!(?:st|nd|rd|th)\b)(?![eE][+-]?\d)([A-Za-z])" "$1 $2")
      ;; Normalize % between two numbers to mod (modulo), standalone % to percent
      (str/replace #"(\d)\s*%\s*(\d)" "$1 mod $2")
      (str/replace #"(\d)\s*%" "$1 percent")
      ;; Collapse whitespace around / between unit words: "meters / second" → "meters/second"
      (str/replace #"([A-Za-z])\s*/\s*([A-Za-z])" "$1/$2")
      ;; Collapse whitespace around / between digits: "21349 /234234" → "21349/234234"
      (str/replace #"(\d)\s*/\s*(\d)" "$1/$2")
      ;; Ensure / and * have spaces between letter and digit: "W/12" → "W / 12", "V*1" → "V * 1"
      (str/replace #"([A-Za-z])\s*[/]\s*(\d)" "$1 / $2")
      (str/replace #"(\d)\s*[/]\s*([A-Za-z])" "$1 / $2")
      (str/replace #"([A-Za-z])\s*[*]\s*(\d)" "$1 * $2")
      (str/replace #"(\d)\s*[*]\s*([A-Za-z])" "$1 * $2")
      (str/replace #"\s+" " ")
      str/trim))

(defn parse-integer [s]
  #?(:cljs (js/parseInt s 10)
     :default (bigint s)))

(defn parse-ratio-token [s]
  (let [[n d] (str/split s #"/")]
    #?(:cljs (m/dec->double (m/ddiv (parse-integer n) (parse-integer d)))
       :default (/ (parse-integer n) (parse-integer d)))))

(defn parse-decimal-token [s]
  #?(:cljs (js/parseFloat s)
     :default (bigdec s)))  ;; Stays as float in CLJS; precision applied in math evaluator

(def number-words
  {"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5
   "six" 6 "seven" 7 "eight" 8 "nine" 9 "ten" 10
   "eleven" 11 "twelve" 12 "thirteen" 13 "fourteen" 14 "fifteen" 15
   "sixteen" 16 "seventeen" 17 "eighteen" 18 "nineteen" 19
   "twenty" 20 "thirty" 30 "forty" 40 "fifty" 50
   "sixty" 60 "seventy" 70 "eighty" 80 "ninety" 90})

(def multiplier-words
  {"hundred" 100 "thousand" 1000 "million" 1000000 "billion" 1000000000})

(defn parse-number-words
  "Parse English number words from tokens starting at index i.
   Returns [value next-index] or nil."
  [tokens i]
  (loop [j i
         total 0
         current 0
         found? false]
    (let [t (some-> (nth tokens j nil) str/lower-case)
          word-val (get number-words t)
          mult-val (get multiplier-words t)]
      (cond
        ;; Skip "and" between number words (e.g. "one hundred and two")
        (and (= "and" t) found?)
        (recur (inc j) total current true)

        ;; A base number word: accumulate into current group
        word-val
        (recur (inc j) total (+ current word-val) true)

        ;; A multiplier: multiply current group and add to total
        (and mult-val found?)
        (let [group (if (zero? current) 1 current)]
          (if (>= mult-val 1000)
            (recur (inc j) (+ total (* group mult-val)) 0 true)
            (recur (inc j) total (* group mult-val) true)))

        ;; Done parsing number words
        found?
        [(+ total current) j]

        :else nil))))

(defn numeric-token? [s]
  (boolean
   (or (re-matches #"\d+" s)
       (re-matches #"\d+\.\d+" s)
       (re-matches #"\d+/\d+" s)
       (re-matches #"(?i)\d+(?:\.\d+)?[eE][+-]?\d+" s))))

(defn parse-sci-token [s]
  #?(:cljs (js/parseFloat s)
     :default (bigdec s)))

(defn parse-number-token [s]
  (cond
    (re-matches #"\d+/\d+" s)
    (parse-ratio-token s)

    (re-matches #"(?i)\d+(?:\.\d+)?[eE][+-]?\d+" s)
    (parse-sci-token s)

    (re-matches #"\d+\.\d+" s)
    (parse-decimal-token s)

    (re-matches #"\d+" s)
    (parse-integer s)

    :else
    nil))

;; ---------------------------------------------------------------------------
;; Simple arithmetic expression evaluator  (+, -, *, /, parens)
;; ---------------------------------------------------------------------------

(declare ^:private math-parse-expr)

(defn- char-at
  "Get the character at index i of string s as a single-char string."
  [s i]
  (subs s i (inc i)))

(defn- digit? [ch]
  (boolean (re-matches #"\d" ch)))

(defn- whitespace? [ch]
  (boolean (re-matches #"\s" ch)))

(defn- alpha? [ch]
  (boolean (re-matches #"[A-Za-z_]" ch)))

(defn- math-tokenize
  "Tokenize a math expression into [:num v], [:op ch], [:lp], [:rp], [:fn name]."
  [s]
  (let [n (count s)]
    (loop [i 0, tokens []]
      (if (>= i n)
        tokens
        (let [ch (char-at s i)]
          (cond
            (whitespace? ch)
            (recur (inc i) tokens)

            ;; Function names and constants
            (alpha? ch)
            (let [end (loop [j (inc i)]
                        (if (and (< j n) (alpha? (char-at s j)))
                          (recur (inc j))
                          j))
                  word (str/lower-case (subs s i end))]
              (cond
                (#{"sqrt" "cbrt" "root"
                   "sin" "cos" "tan" "asin" "acos" "atan"
                   "arcsin" "arccos" "arctan"} word)
                (recur end (conj tokens [:fn word]))

                (#{"rad" "radians" "deg" "degrees"} word)
                (recur end (conj tokens [:angle (if (#{"rad" "radians"} word) :rad :deg)]))

                (contains? math-constants word)
                (recur end (conj tokens [:num (get math-constants word)]))

                :else nil))

            (or (digit? ch) (= ch "."))
            (let [end (loop [j (inc i)]
                        (if (and (< j n)
                                 (let [c (char-at s j)]
                                   (or (digit? c) (= c "."))))
                          (recur (inc j))
                          j))
                  ;; Consume scientific notation suffix: e/E followed by optional +/- and digits
                  end (if (and (< end n)
                              (#{"e" "E"} (char-at s end)))
                        (let [after-e (inc end)
                              after-sign (if (and (< after-e n)
                                                  (#{"+" "-"} (char-at s after-e)))
                                           (inc after-e)
                                           after-e)
                              digit-end (loop [k after-sign]
                                          (if (and (< k n) (digit? (char-at s k)))
                                            (recur (inc k))
                                            k))]
                          (if (> digit-end after-sign)
                            digit-end
                            end))
                        end)
                  ns (subs s i end)
                  v  (if (or (str/includes? ns ".") (re-find #"[eE]" ns))
                       (parse-sci-token ns)
                       (parse-integer ns))]
              (recur end (conj tokens [:num v])))

            (= ch "(") (recur (inc i) (conj tokens [:lp]))
            (= ch ")") (recur (inc i) (conj tokens [:rp]))

            (= ch ",") (recur (inc i) (conj tokens [:comma]))

            (#{"+" "*" "/" "^" "%"} ch)
            (recur (inc i) (conj tokens [:op ch]))

            (contains? math-constants ch)
            (recur (inc i) (conj tokens [:num (get math-constants ch)]))

            (= ch "-")
            (if (or (empty? tokens) (#{:lp :op :comma} (first (peek tokens))))
              ;; Unary minus: absorb into next number
              (let [j   (inc i)
                    end (loop [k j]
                          (if (and (< k n)
                                   (let [c (char-at s k)]
                                     (or (digit? c) (= c "."))))
                            (recur (inc k))
                            k))]
                (if (> end j)
                  (let [ns (subs s i end)
                        v  (if (str/includes? ns ".")
                             (parse-decimal-token ns)
                             (parse-integer ns))]
                    (recur end (conj tokens [:num v])))
                  nil))
              (recur (inc i) (conj tokens [:op ch])))

            :else nil))))))

(defn- math-nth-root
  "Compute the nth root of x. Returns exact integer when possible, else decimal."
  [x n]
  #?(:clj
     (let [xd (double x)]
       (cond
         (zero? xd) 0N
         :else
         (let [approx (Math/pow (Math/abs xd) (/ 1.0 (double n)))
               candidate (Math/round approx)]
           (if (and (pos? xd)
                    (pos? candidate)
                    (= (reduce *' (repeat (long n) (bigint candidate)))
                       (bigint x)))
             (bigint candidate)
             (bigdec (Math/pow xd (/ 1.0 (double n))))))))
     :cljs
     (let [result (m/dpow x (m/ddiv 1.0 n))
           rounded (m/dround result)]
       (if (m/d== result rounded)
         (m/dec->double rounded)
         (m/dec->double result)))
     :lpy
     (let [xd (double x)]
       (cond
         (zero? xd) 0N
         :else
         (let [approx (math/pow (abs xd) (/ 1.0 (double n)))
               candidate (long (round approx))]
           (if (and (pos? xd)
                    (pos? candidate)
                    (= (reduce * 1N (repeat (long n) (bigint candidate)))
                       (bigint x)))
             (bigint candidate)
             (bigdec (math/pow xd (/ 1.0 (double n))))))))))

(defn- math-deg->rad [x]
  (* (m/dec->double x) (/ m/PI 180.0)))

(def ^:private forward-math-trig
  #{"sin" "cos" "tan"})

(def ^:private inverse-math-trig
  #{"asin" "acos" "atan" "arcsin" "arccos" "arctan"})

(def ^:private math-trig-fns
  {"sin" m/dsin "cos" m/dcos "tan" m/dtan
   "asin" m/dasin "acos" m/dacos "atan" m/datan
   "arcsin" m/dasin "arccos" m/dacos "arctan" m/datan})

(defn- math-trig-eval
  "Evaluate a trig function. angle-mode is :deg (default) or :rad."
  [fname v angle-mode]
  (let [f (get math-trig-fns fname)
        mode (or angle-mode :deg)
        input (if (forward-math-trig fname)
                (if (= mode :rad) (m/dec->double v) (math-deg->rad v))
                (m/dec->double v))
        raw-result (double (f input))
        result (if (inverse-math-trig fname)
                 (if (= mode :rad) raw-result (* (/ 180.0 m/PI) raw-result))
                 raw-result)
        rounded (#?(:lpy round :default Math/round) (* result 1e10))]
    (if (< (#?(:lpy abs :default Math/abs) result) 1e-14)
      #?(:cljs 0 :default 0N)
      (if (< (#?(:lpy abs :default Math/abs) (- result (/ (double rounded) 1e10))) 1e-14)
        #?(:cljs (/ rounded 1e10) :default (bigdec (/ (double rounded) 1e10)))
        #?(:cljs result :default (bigdec result))))))

(defn- math-parse-factor [tokens pos]
  (when (< pos (count tokens))
    (case (first (nth tokens pos))
      :num [(second (nth tokens pos)) (inc pos)]
      :fn  (let [fname (second (nth tokens pos))
                 npos (inc pos)
                 has-paren (and (< npos (count tokens))
                                (= :lp (first (nth tokens npos))))]
             (cond
               ;; Parenthesized form: fn(expr) or fn(expr, expr)
               has-paren
               (case fname
                 ;; sqrt(expr)
                 "sqrt"
                 (when-let [[v p1] (math-parse-expr tokens (inc npos))]
                   (when (and (< p1 (count tokens))
                              (= :rp (first (nth tokens p1))))
                     [(math-nth-root v 2) (inc p1)]))

                 ;; cbrt(expr)
                 "cbrt"
                 (when-let [[v p1] (math-parse-expr tokens (inc npos))]
                   (when (and (< p1 (count tokens))
                              (= :rp (first (nth tokens p1))))
                     [(math-nth-root v 3) (inc p1)]))

                 ;; root(n, expr)
                 "root"
                 (when-let [[degree p1] (math-parse-expr tokens (inc npos))]
                   (when (and (< p1 (count tokens))
                              (= :comma (first (nth tokens p1))))
                     (when-let [[v p2] (math-parse-expr tokens (inc p1))]
                       (when (and (< p2 (count tokens))
                                  (= :rp (first (nth tokens p2))))
                         [(math-nth-root v degree) (inc p2)]))))

                 ;; Trig functions: sin(x), cos(x), etc. — degrees by default
                 ("sin" "cos" "tan" "asin" "acos" "atan"
                  "arcsin" "arccos" "arctan")
                 (when-let [[v p1] (math-parse-expr tokens (inc npos))]
                   ;; Check for optional angle mode before closing paren
                   (let [[mode p2] (if (and (< p1 (count tokens))
                                            (= :angle (first (nth tokens p1))))
                                     [(second (nth tokens p1)) (inc p1)]
                                     [nil p1])]
                     (when (and (< p2 (count tokens))
                                (= :rp (first (nth tokens p2))))
                       [(math-trig-eval fname v mode) (inc p2)])))

                 nil)

               ;; Bare trig: sin 45, cos 60 (no parens, consume next factor)
               ;; Check for optional angle mode suffix: sin 45 rad, cos 60 deg
               (contains? math-trig-fns fname)
               (when-let [[v p1] (math-parse-factor tokens npos)]
                 (let [[mode p2] (if (and (< p1 (count tokens))
                                          (= :angle (first (nth tokens p1))))
                                   [(second (nth tokens p1)) (inc p1)]
                                   [nil p1])]
                   [(math-trig-eval fname v mode) p2]))

               :else nil))
      :lp  (when-let [[v npos] (math-parse-expr tokens (inc pos))]
             (when (and (< npos (count tokens))
                        (= :rp (first (nth tokens npos))))
               [v (inc npos)]))
      nil)))

(defn- max-pow-exponent []
  #?(:cljs (m/get-precision)
     :default 10000))

(defn- math-pow [base exp]
  #?(:clj
     (let [n (long exp)]
       (when (> (Math/abs n) (max-pow-exponent))
         (throw (ex-info "Exponent too large" {:error :exponent-too-large :exp n
                                               :limit (max-pow-exponent)})))
       (cond
         (zero? n) 1N
         (pos? n)  (reduce * 1N (repeat n base))
         :else     (/ 1N (math-pow base (- n)))))
     :cljs
     (let [limit (max-pow-exponent)]
       (when (> (Math/abs (m/dec->double exp)) limit)
         (throw (ex-info "Exponent too large" {:error :exponent-too-large :exp exp
                                               :limit limit})))
       (m/dpow base exp))
     :lpy
     (let [n (long exp)]
       (when (> (abs n) (max-pow-exponent))
         (throw (ex-info "Exponent too large" {:error :exponent-too-large :exp n
                                               :limit (max-pow-exponent)})))
       (cond
         (zero? n) 1N
         (pos? n)  (reduce * 1N (repeat n base))
         :else     (/ 1N (math-pow base (- n)))))))

(defn- math-parse-power
  "Parse factor (^ factor)* — right-associative."
  [tokens pos]
  (when-let [[base p0] (math-parse-factor tokens pos)]
    (if (and (< p0 (count tokens))
             (= [:op "^"] (nth tokens p0)))
      (when-let [[exp p1] (math-parse-power tokens (inc p0))]
        [(math-pow base exp) p1])
      [base p0])))

(defn- math-div [a b]
  #?(:clj  (try (/ a b)
                (catch ArithmeticException _
                  (.divide (bigdec a) (bigdec b) (java.math.MathContext. 34))))
     :cljs (m/ddiv a b)
     :lpy  (/ a b)))

(defn- math-parse-term [tokens pos]
  (when-let [[v0 p0] (math-parse-power tokens pos)]
    (loop [acc v0, p p0]
      (if (and (< p (count tokens))
               (= :op (first (nth tokens p)))
               (#{"*" "/" "%"} (second (nth tokens p))))
        (let [op (second (nth tokens p))]
          (if-let [[v2 p2] (math-parse-power tokens (inc p))]
            (recur (case op
                     "*" (m/d* acc v2)
                     "/" (math-div acc v2)
                     "%" (m/dmod acc v2))
                   p2)
            [acc p]))
        [acc p]))))

(defn- math-parse-expr [tokens pos]
  (when-let [[v0 p0] (math-parse-term tokens pos)]
    (loop [acc v0, p p0]
      (if (and (< p (count tokens))
               (= :op (first (nth tokens p)))
               (#{"+" "-"} (second (nth tokens p))))
        (let [op (second (nth tokens p))]
          (if-let [[v2 p2] (math-parse-term tokens (inc p))]
            (recur (if (= "+" op) (m/d+ acc v2) (m/d- acc v2)) p2)
            [acc p]))
        [acc p]))))

(defn- standalone-trig?
  "True when tokens represent a single trig call with no binary operators
   and no nested function calls, e.g. sin(30), asin 0.5, cos 45 deg.
   Also matches trig with pi division: cos pi/4 → [:fn] [:num pi] [:op /] [:num 4].
   These should be handled by parse-trig (which tracks angle mode and
   attaches unit labels) rather than parse-math."
  [tokens]
  (and (seq tokens)
       (= :fn (first (first tokens)))
       (contains? math-trig-fns (second (first tokens)))
       ;; If there are nested function calls, it's a compound expression
       (<= (count (filter #(= :fn (first %)) tokens)) 1)
       ;; No binary operators, OR the only operator is / in a pi/N pattern
       (or (not (some #(= :op (first %)) tokens))
           ;; Allow pi/N: [:fn] [:num pi-val] [:op "/"] [:num N]
           (and (= 4 (count tokens))
                (= [:op "/"] (nth tokens 2))
                (= :num (first (nth tokens 1)))
                (= (m/dec->double pi-value) (m/dec->double (second (nth tokens 1))))))))

(defn- annotate-trig-expr
  "Annotate a trig expression string with explicit angle markers.
   'cos 32 / sin 45'       → 'cos 32° / sin 45°'
   'cos 32 rad / sin 45 rad' → unchanged (already explicit)
   'sin(45) + cos(60)'     → 'sin(45°) + cos(60°)'"
  [s]
  (-> s
      ;; Bare form: sin 30 → sin 30°, sin 30 rad → sin 30 rad (unchanged)
      (str/replace #"(?i)\b(a?(?:rc)?(?:sin|cos|tan))\s+(\d+(?:\.\d+)?)(\s+(?:rad|radians|deg|degrees)\b)?"
                   (fn [[_ fname num mode]]
                     (if mode
                       (str fname " " num mode)
                       (str fname " " num "°"))))
      ;; Paren form: sin(30) → sin(30°), sin(30 rad) → sin(30 rad)
      (str/replace #"(?i)\b(a?(?:rc)?(?:sin|cos|tan))\((\d+(?:\.\d+)?)\s*((?:rad|radians|deg|degrees))?\)"
                   (fn [[_ fname num mode]]
                     (if mode
                       (str fname "(" num " " mode ")")
                       (str fname "(" num "°)"))))
      str/trim))

(defn parse-math
  "Evaluate a simple arithmetic expression string. Returns a number or nil.
   When the expression contains trig functions, returns a map
   {:value N :trig-expr \"annotated string\"} instead of a bare number."
  [s]
  (when-let [tokens (math-tokenize (str/trim s))]
    (when (and (seq tokens) (not (standalone-trig? tokens)))
      (let [[v pos] (math-parse-expr tokens 0)]
        (when (and v (= pos (count tokens)))
          (let [v (m/normalize v)]
            (if (some #(and (= :fn (first %))
                           (contains? math-trig-fns (second %))) tokens)
              {:math-value v :trig-expr (annotate-trig-expr (str/trim s))}
              v)))))))

(defn math-value
  "Extract the numeric value from a parse-math result (bare number or trig map)."
  [result]
  (if (map? result) (:math-value result) result))

(defn evaluate-math-exprs
  "Replace parenthesised arithmetic expressions in `s` with their values.
   Skips parentheses immediately preceded by function names (sqrt, cbrt, root)."
  [s]
  (let [result (str/replace s #"(?<![A-Za-z])\(([^()]+)\)"
                            (fn [[match inner]]
                              (if-let [v (math-value (parse-math inner))]
                                (str v)
                                match)))]
    (if (= result s)
      s
      (recur result))))

(defn- try-parse-math-tokens
  "Greedily collect num op num op num ... from `tokens` starting at `i`.
   Operators are +, -, * (not / to avoid ambiguity with unit division).
   Returns [value next-index] when at least one operator was consumed, else nil."
  [tokens i]
  (loop [j i, parts []]
    (let [tok (nth tokens j nil)]
      (if (even? (count parts))
        ;; Expecting a number
        (if (and tok (numeric-token? tok))
          (recur (inc j) (conj parts tok))
          (when (>= (count parts) 3)
            (when-let [v (math-value (parse-math (str/join " " parts)))]
              [v j])))
        ;; Expecting an operator (+, -, *)
        (if (and tok (= 1 (count tok)) (#{"+" "-" "*" "^" "%"} tok))
          (recur (inc j) (conj parts tok))
          (when (>= (count parts) 3)
            (when-let [v (math-value (parse-math (str/join " " parts)))]
              [v j])))))))

(def ordinal-fractions
  {"half"       #?(:cljs 0.5     :default 1/2)
   "third"      #?(:cljs (/ 1 3) :default 1/3)
   "quarter"    #?(:cljs 0.25    :default 1/4)
   "fifth"      #?(:cljs 0.2     :default 1/5)
   "sixth"      #?(:cljs (/ 1 6) :default 1/6)
   "seventh"    #?(:cljs (/ 1 7) :default 1/7)
   "eighth"     #?(:cljs 0.125   :default 1/8)
   "ninth"      #?(:cljs (/ 1 9) :default 1/9)
   "tenth"      #?(:cljs 0.1     :default 1/10)
   "sixteenth"  #?(:cljs 0.0625  :default 1/16)})

(defn parse-number-at [tokens i]
  (let [raw (nth tokens i nil)
        t (some-> raw str/lower-case)
        t2 (some-> (nth tokens (inc i) nil) str/lower-case)]
    (cond
      (and (#{"a" "an"} raw) (contains? ordinal-fractions t2))
      [(ordinal-fractions t2) (+ i 2)]

      (#{"a" "an"} raw)
      [1 (inc i)]

      (contains? ordinal-fractions t)
      [(ordinal-fractions t) (inc i)]

      ;; Plain number, possibly followed by mixed fraction or math operators
      (some? (parse-number-token t))
      (or (try-parse-math-tokens tokens i)
          (if (and (some? t2) (re-matches #"\d+/\d+" t2))
            [(m/dec->double (m/d+ (parse-number-token t) (parse-number-token t2)))
             (+ i 2)]
            [(parse-number-token t) (inc i)]))

      ;; Single token containing math (no spaces): 2+2, 3*4-1
      (and (some? t) (some? (math-value (parse-math t))))
      [(math-value (parse-math t)) (inc i)]

      ;; Math constants: pi, π, e, phi, φ
      (contains? math-constants t)
      [(get math-constants t) (inc i)]

      ;; Pi division as single token: "pi/4"
      (and t (re-matches #"(?i)(?:pi|π)/\d+(?:\.\d+)?" t))
      (let [[_ denom-str] (re-matches #"(?i)(?:pi|π)/(.+)" t)
            v #?(:cljs (js/parseFloat denom-str) :default (bigdec denom-str))]
        [#?(:cljs (m/dec->double (m/ddiv pi-value v))
            :default (m/ddiv pi-value v))
         (inc i)])

      ;; English number words: "ten", "twenty three", "one hundred", etc.
      (parse-number-words tokens i)
      (parse-number-words tokens i)

      :else
      nil)))

(defn normalize-unit-token [s]
  (let [raw (-> s
                str/trim
                (str/replace #"^[^\w]+|[^\w]+$" ""))
        k   (str/lower-case raw)]
    (or (get unit-aliases raw)
        (get unit-aliases k)
        (throw (ex-info "Unknown unit"
                        {:error :unknown-unit
                         :unit s})))))

(defn unit-map [u exp]
  (cond
    (keyword? u) {u exp}
    (map? u) (into {} (map (fn [[k v]] [k (* exp v)]) u))  ;; dim exponents are small ints
    :else (throw (ex-info "Bad unit" {:unit u}))))

(defn merge-unit-maps [& maps]
  (->> maps
       (apply merge-with +)
       (remove (fn [[_ v]] (zero? v)))
       (into {})))

(defn- parse-int-str [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s 10)
     :lpy (int s)))

(defn parse-component-token [token]
  (let [raw token
        lower (str/lower-case raw)]
    (cond
      (contains? special-unit-forms raw)
      (get special-unit-forms raw)

      (contains? special-unit-forms lower)
      (get special-unit-forms lower)

      (str/includes? raw "/")
      (let [[num den] (str/split raw #"/" 2)]
        (merge-unit-maps
         (unit-map (parse-component-token num) 1)
         (unit-map (parse-component-token den) -1)))

      (str/includes? raw "*")
      (let [parts (str/split raw #"\*")]
        (apply merge-unit-maps
               (map #(unit-map (parse-component-token %) 1) parts)))

      (re-matches #"(?i).+\^-?\d+" raw)
      (let [[base exp] (str/split raw #"\^")]
        (unit-map (normalize-unit-token base)
                  (parse-int-str exp)))

      :else
      (normalize-unit-token raw))))

(defn simple-unit-result [components]
  (if (= 1 (count components))
    (first components)
    (apply merge-unit-maps (map #(unit-map % 1) components))))

(def ^:private compound-prefixes
  #{"nautical" "metric" "short" "fluid" "fl"})

(defn- join-compound-tokens
  "Join adjacent tokens where the first is a known compound prefix
   (e.g. 'nautical' 'mile' → 'nautical mile')."
  [tokens]
  (loop [remaining (seq tokens) result []]
    (if-not remaining
      result
      (let [t (first remaining)
            nxt (second remaining)]
        (if (and nxt (compound-prefixes (str/lower-case t)))
          (recur (nnext remaining) (conj result (str t " " nxt)))
          (recur (next remaining) (conj result t)))))))

(defn parse-unit-product [tokens]
  (let [tokens (->> tokens
                    (remove #(#{"a" "an"} %))
                    join-compound-tokens)
        components (map parse-component-token tokens)]
    (simple-unit-result components)))

(defn- vec-index-of [v val]
  (loop [i 0]
    (cond
      (>= i (count v)) -1
      (= (nth v i) val) i
      :else (recur (inc i)))))

(defn parse-unit-phrase [s]
  (let [tokens (->> (str/split (str/trim s) #"\s+")
                    (remove str/blank?)
                    vec)
        lower-tokens (mapv str/lower-case tokens)]
    (cond
      (empty? tokens)
      (throw (ex-info "Missing unit" {:error :missing-unit}))

      ;; "time" is shorthand for "hours, minutes, and seconds"
      (= ["time"] lower-tokens)
      [{:hr 1} {:min 1} {:s 1}]

      ;; Mixed output target: "feet and inches", "hours, minutes, and seconds"
      ;; Commas are stripped by clean-phrase, so "hours, minutes, and seconds"
      ;; becomes "hours minutes and seconds". We split on "and" first, then
      ;; expand multi-token groups into individual units (unless they form a
      ;; known compound like "nautical miles").
      (some #{"and"} lower-tokens)
      (let [;; Split on "and" into groups of tokens
            groups (loop [remaining tokens, current [], groups []]
                     (if (empty? remaining)
                       (if (seq current)
                         (conj groups current)
                         groups)
                       (if (= "and" (str/lower-case (first remaining)))
                         (recur (rest remaining)
                                []
                                (if (seq current)
                                  (conj groups current)
                                  groups))
                         (recur (rest remaining)
                                (conj current (first remaining))
                                groups))))
            ;; Expand multi-token groups: try as compound unit first,
            ;; if it produces a multi-dimensional result, split into individual tokens
            units (reduce
                   (fn [acc group]
                     (if (= 1 (count group))
                       (conj acc (first group))
                       ;; Multi-token group — check if it's a compound unit
                       (let [joined (str/join " " group)]
                         (if (try (normalize-unit-token joined) true
                                  #?(:cljs (catch :default _ false)
                                     :default (catch Exception _ false)))
                           (conj acc joined)
                           (into acc group)))))
                   []
                   groups)]
        (if (>= (count units) 2)
          (mapv #(parse-unit-phrase %) units)
          (parse-unit-product tokens)))

      (#{"square" "sq" "squared"} (first lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (rest tokens))) 2)

      (#{"cubic" "cu" "cubed"} (first lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (rest tokens))) 3)

      (#{"square" "squared"} (last lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (butlast tokens))) 2)

      (#{"cubic" "cubed"} (last lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (butlast tokens))) 3)

      (some #{"per"} lower-tokens)
      (let [i (vec-index-of lower-tokens "per")
            num-tokens (subvec tokens 0 i)
            den-tokens (subvec tokens (inc i))]
        (merge-unit-maps
         (unit-map (parse-unit-product num-tokens) 1)
         (unit-map (parse-unit-product den-tokens) -1)))

      (some #{"/"} lower-tokens)
      (let [i (vec-index-of lower-tokens "/")
            num-tokens (subvec tokens 0 i)
            den-tokens (subvec tokens (inc i))]
        (merge-unit-maps
         (unit-map (parse-unit-product num-tokens) 1)
         (unit-map (parse-unit-product den-tokens) -1)))

      :else
      (parse-unit-product tokens))))

(defn next-number-index [tokens start]
  (loop [i start]
    (cond
      (>= i (count tokens)) nil
      (parse-number-at tokens i) i
      :else (recur (inc i)))))

(defn- split-on-qty-ops
  "Split tokens on *, /, +, or - that appear between quantity groups.
   A token is a quantity operator when it is followed by something that
   starts a number.  Returns {:segments [[tok ...] ...] :ops [:* :/ :+ or :- ...]}
   or nil when no quantity-level operators are found."
  [tokens]
  (loop [i 0, current [], segments [], ops []]
    (cond
      (>= i (count tokens))
      (when (seq ops)
        {:segments (conj segments current) :ops ops})

      (and (#{"*" "/" "+" "-"} (nth tokens i))
           (seq current)
           (< (inc i) (count tokens))
           (some? (parse-number-at tokens (inc i))))
      (recur (inc i) [] (conj segments current)
             (conj ops (case (nth tokens i)
                         "*" :*
                         "/" :/
                         "+" :+
                         "-" :-)))

      :else
      (recur (inc i) (conj current (nth tokens i)) segments ops))))

(defn- parse-qty-segment
  "Parse a single quantity segment (number + optional unit).
   Returns {:value N :unit U} or nil."
  [seg-tokens]
  (when-let [[value j] (parse-number-at seg-tokens 0)]
    (let [j (if (#{"a" "an"} (nth seg-tokens j nil))
              (inc j)
              j)]
      (if (>= j (count seg-tokens))
        {:value value :unit {}}
        {:value value :unit (parse-unit-phrase
                             (str/join " " (subvec seg-tokens j)))}))))

(defn parse-quantity [s]
  (let [tokens (->> (str/split (str/trim s) #"\s+")
                    (remove str/blank?)
                    vec)]
    ;; Try quantity arithmetic: "100 MB / 100 Mbps", "60 mph * 2 hours"
    (or (when-let [{:keys [segments ops]} (split-on-qty-ops tokens)]
          (let [parsed (mapv parse-qty-segment segments)]
            (when (and (every? some? parsed)
                       ;; For +/- all operands must have units — otherwise
                       ;; it's plain scalar math (e.g. "3 + 2 hours").
                       ;; For * the first operand must have a unit, unless
                       ;; there's also a / (e.g. "4.5 / 77 days/meter").
                       (if (every? #{:+ :-} ops)
                         (every? #(not= {} (:unit %)) parsed)
                         (or (not= {} (:unit (first parsed)))
                             (some #{:/} ops))))
              {:qty-expr true :terms parsed :ops ops})))
        ;; Existing: simple or mixed quantities
        (loop [i 0
               terms []]
          (if (>= i (count tokens))
            (cond
              (empty? terms)
              (throw (ex-info "Unparseable quantity"
                              {:error :unparseable-quantity
                               :quantity s}))

              (= 1 (count terms))
              (first terms)

              :else
              terms)

            (let [[value j] (or (parse-number-at tokens i)
                                (throw (ex-info "Expected number"
                                                {:error :expected-number
                                                 :token (nth tokens i nil)})))
                  ;; Allows "half a gallon"
                  j (if (#{"a" "an"} (nth tokens j nil))
                      (inc j)
                      j)
                  next-i (next-number-index tokens j)
                  unit-tokens (subvec tokens j (or next-i (count tokens)))
                  unit-str (str/join " " unit-tokens)]
              (recur (or next-i (count tokens))
                     (conj terms {:value value
                                  :unit (parse-unit-phrase unit-str)}))))))))

(defn- parse-long-str [s]
  #?(:clj (Long/parseLong s)
     :cljs (js/parseInt s 10)
     :lpy (int s)))

(defn extract-format [s]
  (cond
    (re-find #"(?i)\s+rounded to \d+ decimals?$" s)
    (let [[_ n] (re-find #"(?i)\s+rounded to (\d+) decimals?$" s)]
      [(str/replace s #"(?i)\s+rounded to \d+ decimals?$" "")
       {:round (parse-long-str n)}])

    (re-find #"(?i)\s+with \d+ sig figs$" s)
    (let [[_ n] (re-find #"(?i)\s+with (\d+) sig figs$" s)]
      [(str/replace s #"(?i)\s+with \d+ sig figs$" "")
       {:sig-figs (parse-long-str n)}])

    (re-find #"(?i)\s+as a fraction$" s)
    [(str/replace s #"(?i)\s+as a fraction$" "")
     {:style :fraction}]

    :else
    [s nil]))

(defn extract-approx [s]
  (let [lower (str/lower-case s)]
    (cond
      (str/starts-with? lower "about ")
      [(subs s 6) true]

      (str/starts-with? lower "roughly ")
      [(subs s 8) true]

      (str/starts-with? lower "~ ")
      [(subs s 2) true]

      :else
      [s false])))

(defn split-request [s]
  (cond
    ;; how many inches [are] in 3 feet
    (re-matches #"(?i)^how many .+(?:\s+are)? in .+$" s)
    (let [[_ to quantity] (re-matches #"(?i)^how many (.+?)(?:\s+are)? in (.+)$" s)]
      [quantity to])

    ;; how many yards is/are 12 feet
    (re-matches #"(?i)^how many .+ (?:is|are) .+$" s)
    (let [[_ to quantity] (re-matches #"(?i)^how many (.+) (?:is|are) (.+)$" s)]
      [quantity to])

    ;; 12 feet is how many yards
    (re-matches #"(?i)^.+ is how many .+$" s)
    (let [[_ quantity to] (re-matches #"(?i)^(.+) is how many (.+)$" s)]
      [quantity to])

    ;; how much is 5 kg in pounds
    (re-matches #"(?i)^how much is .+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^how much is (.+) (in|to) (.+)$" s)]
      [quantity to])

    ;; what is 12 ft in yards
    (re-matches #"(?i)^what is .+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^what is (.+) (in|to) (.+)$" s)]
      [quantity to])

    ;; convert 12 feet to yards
    (re-matches #"(?i)^convert .+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^convert (.+) (in|to) (.+)$" s)]
      [quantity to])

    ;; 12 ft in yards / 5 miles to km
    (re-matches #"(?i)^.+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^(.+) (in|to) (.+)$" s)]
      [quantity to])

    :else
    nil))

(defn parse-error [phrase ex]
  (let [data (ex-data ex)]
    (case (:error data)
      :unknown-unit
      {:error :unknown-unit
       :unit (:unit data)
       :phrase phrase}

      :ambiguous-quantities
      {:error :ambiguous-quantities
       :phrase phrase}

      (throw ex))))

(defn- try-parse-quantity [s]
  (try
    (parse-quantity s)
    true
    #?(:cljs (catch :default _ false)
       :default (catch Exception _ false))))

(defn- try-swap-sides [quantity-str to-str]
  (try
    (parse-quantity quantity-str)
    [quantity-str to-str]
    #?(:cljs
       (catch :default _
         (try
           (parse-quantity to-str)
           [to-str quantity-str]
           (catch :default _
             [quantity-str to-str])))
       :default
       (catch Exception _
         (try
           (parse-quantity to-str)
           [to-str quantity-str]
           (catch Exception _
             [quantity-str to-str]))))))

(defn split-display-parts
  "Extract {:from ... :target ...} from the input, where :from is the
   quantity side and :target is the unit-only side. Used for display formatting."
  [input]
  (or
   ;; how many X [are] in Y → from=Y, target=X
   (when-let [[_ to from] (re-matches #"(?i)^how many (.+?)(?:\s+are)? in (.+)$" input)]
     {:from (str/trim from) :target (str/trim to)})
   ;; how many X is/are Y → from=Y, target=X
   (when-let [[_ to from] (re-matches #"(?i)^how many (.+) (?:is|are) (.+)$" input)]
     {:from (str/trim from) :target (str/trim to)})
   ;; Y is how many X → from=Y, target=X
   (when-let [[_ from to] (re-matches #"(?i)^(.+) is how many (.+)$" input)]
     {:from (str/trim from) :target (str/trim to)})
   ;; Generic "X in/to Y" (with optional leading "how much is"/"what is"/"convert")
   (when-let [[_ lhs _ rhs] (re-matches #"(?i)^(?:(?:how much is|what is|convert)\s+)?(.+?)\s+(in|to)\s+(.+?)$" input)]
     {:from (str/trim lhs) :target (str/trim rhs)})))

(defn- parse-percentage-number
  "Parse a number from the start of a string, returning [value remaining-str] or nil."
  [s]
  (let [s (str/trim s)
        tokens (->> (str/split s #"\s+") (remove str/blank?) vec)]
    (when-let [[value j] (parse-number-at tokens 0)]
      [value (str/trim (str/join " " (subvec tokens j)))])))

(defn parse-percentage
  "Try to parse a percentage expression. Returns a request map or nil.
   Supports:
     'X is what percent of Y'  → {:op :percentage :type :what-percent :value X :total Y}
     'what percent of Y is X'  → same
     'what percentage of Y is X' → same
     'X percent of Y'          → {:op :percentage :type :percent-of :percent X :value Y}
     'X% of Y'                 → same (% normalized to 'percent' by clean-phrase)"
  [s]
  (let [lower (str/lower-case s)]
    (or
     ;; "X is what percent of Y" / "X is what percentage of Y"
     (when-let [[_ x-str y-str] (re-matches #"(?i)^(.+?)\s+is\s+what\s+percent(?:age)?\s+of\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :what-percent :value x :total y})))

     ;; "what percent of Y is X" / "what percentage of Y is X"
     (when-let [[_ y-str x-str] (re-matches #"(?i)^what\s+percent(?:age)?\s+of\s+(.+?)\s+is\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :what-percent :value x :total y})))

     ;; "what is X percent of Y"
     (when-let [[_ x-str y-str] (re-matches #"(?i)^what\s+is\s+(.+?)\s+percent\s+of\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :percent-of :percent x :value y})))

     ;; "X percent of Y"
     (when-let [[_ x-str y-str] (re-matches #"(?i)^(.+?)\s+percent\s+of\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :percent-of :percent x :value y}))))))

(def ^:private ordinal-to-int
  {"2nd" 2 "3rd" 3 "4th" 4 "5th" 5 "6th" 6 "7th" 7 "8th" 8 "9th" 9 "10th" 10
   "second" 2 "third" 3 "fourth" 4 "fifth" 5 "sixth" 6 "seventh" 7 "eighth" 8 "ninth" 9 "tenth" 10})

(defn parse-root
  "Try to parse a root expression. Returns a request map or nil.
   Supports:
     'square root of 144'          → {:op :root :degree 2 :value 144}
     'cube root of 27'             → {:op :root :degree 3 :value 27}
     'sqrt 144' / 'sqrt of 144'    → {:op :root :degree 2 :value 144}
     'cbrt 27' / 'cbrt of 27'      → {:op :root :degree 3 :value 27}
     '4th root of 16'              → {:op :root :degree 4 :value 16}
     'what is the square root of 25' → {:op :root :degree 2 :value 25}
     'fifth root of 32'            → {:op :root :degree 5 :value 32}"
  [s]
  (let [lower (str/lower-case s)]
    (or
     ;; "what is the Nth root of X" / "what is the square root of X"
     (when-let [[_ deg-str val-str] (re-matches #"(?i)^(?:what\s+is\s+(?:the\s+)?)(.+?)\s+root\s+of\s+(.+)$" s)]
       (let [deg-lower (str/lower-case (str/trim deg-str))]
         (when-let [degree (case deg-lower
                             "square" 2
                             "sq" 2
                             "cube" 3
                             (or (get ordinal-to-int deg-lower)
                                 (parse-number-token deg-lower)))]
           (when-let [[value _] (parse-percentage-number val-str)]
             {:op :root :degree (long degree) :value value}))))

     ;; "square root of X"
     (when-let [[_ val-str] (re-matches #"(?i)^square\s+root\s+of\s+(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 2 :value value}))

     ;; "cube root of X"
     (when-let [[_ val-str] (re-matches #"(?i)^cube\s+root\s+of\s+(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 3 :value value}))

     ;; "Nth root of X" (ordinal or numeric)
     (when-let [[_ deg-str val-str] (re-matches #"(?i)^(\S+)\s+root\s+of\s+(.+)$" s)]
       (let [deg-lower (str/lower-case deg-str)]
         (when-let [degree (or (get ordinal-to-int deg-lower)
                               (parse-number-token deg-lower))]
           (when-let [[value _] (parse-percentage-number val-str)]
             {:op :root :degree (long degree) :value value}))))

     ;; "sqrt X" / "sqrt of X"
     (when-let [[_ val-str] (re-matches #"(?i)^sqrt\s+(?:of\s+)?(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 2 :value value}))

     ;; "cbrt X" / "cbrt of X"
     (when-let [[_ val-str] (re-matches #"(?i)^cbrt\s+(?:of\s+)?(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 3 :value value})))))

(defn- parse-pi-expr
  "Parse a pi expression like 'pi', 'pi/4', '2pi', '2*pi'.
   Returns [value next-index] consuming tokens from `tokens` at index `i`, or nil."
  [tokens i]
  (let [t (some-> (nth tokens i nil) str/lower-case)]
    (when (#{"pi" "π"} t)
      (let [next-t (some-> (nth tokens (inc i) nil) str/lower-case)]
        (if (and next-t (re-matches #"\d+(?:\.\d+)?" next-t))
          ;; "pi 4" is not valid, just return pi
          [pi-value (inc i)]
          [pi-value (inc i)])))))

(defn- parse-trig-value
  "Parse the value argument for a trig function from tokens starting at index i.
   Handles pi, pi/N, numbers, and number expressions.
   Returns [value angle-mode next-index] or nil."
  [tokens i]
  (let [t (some-> (nth tokens i nil) str/lower-case)
        t2 (some-> (nth tokens (inc i) nil) str/lower-case)
        t3 (some-> (nth tokens (+ i 2) nil) str/lower-case)]
    (cond
      ;; "pi/N" as a single token like "pi/4"
      (and t (re-matches #"(?i)(?:pi|π)/\d+(?:\.\d+)?" t))
      (let [[_ denom-str] (re-matches #"(?i)(?:pi|π)/(.+)" t)
            v #?(:cljs (js/parseFloat denom-str) :default (bigdec denom-str))]
        [#?(:cljs (m/dec->double (m/ddiv pi-value v))
            :default (m/ddiv pi-value v))
         :rad (inc i)])

      ;; "pi" "/" "N" as separate tokens (clean-phrase splits "pi/4" → "pi / 4")
      (and (#{"pi" "π"} t) (= "/" t2) t3 (re-matches #"\d+(?:\.\d+)?" t3))
      (let [v #?(:cljs (js/parseFloat t3) :default (bigdec t3))]
        [#?(:cljs (m/dec->double (m/ddiv pi-value v))
            :default (m/ddiv pi-value v))
         :rad (+ i 3)])

      ;; bare "pi"
      (#{"pi" "π"} t)
      [pi-value :rad (inc i)]

      ;; numeric value
      :else
      (when-let [[value j] (parse-number-at tokens i)]
        [value nil j]))))

(def ^:private trig-fns
  {"sin" :sin "cos" :cos "tan" :tan
   "asin" :asin "acos" :acos "atan" :atan
   "arcsin" :asin "arccos" :acos "arctan" :atan})

(def ^:private inverse-trig-fns
  #{:asin :acos :atan})

(defn parse-trig
  "Try to parse a trig expression. Returns a request map or nil.
   Supports:
     'sin 45'              → {:op :trig :fn :sin :value 45 :angle-mode :deg}
     'sin 45 degrees'      → same
     'sin 1 rad'           → {:op :trig :fn :sin :value 1 :angle-mode :rad}
     'sin pi'              → {:op :trig :fn :sin :value π :angle-mode :rad}
     'sin pi/4'            → {:op :trig :fn :sin :value π/4 :angle-mode :rad}
     'asin 0.5'            → {:op :trig :fn :asin :value 0.5 :angle-mode :deg}
     'asin 0.5 in radians' → {:op :trig :fn :asin :value 0.5 :angle-mode :rad}"
  [s]
  (let [tokens (->> (str/split (str/trim s) #"\s+")
                    (remove str/blank?)
                    vec)
        fn-name (some-> (first tokens) str/lower-case)]
    (when-let [trig-fn (get trig-fns fn-name)]
      (when (> (count tokens) 1)
        (when-let [[value pi-mode j] (parse-trig-value tokens 1)]
          (let [remaining (subvec tokens j)
                lower-remaining (mapv str/lower-case remaining)
                ;; Determine angle mode from remaining tokens
                [angle-mode consumed]
                (cond
                  ;; "in radians" / "in rad" (for inverse trig output)
                  (and (>= (count lower-remaining) 2)
                       (= "in" (first lower-remaining))
                       (#{"radians" "rad"} (second lower-remaining)))
                  [:rad 2]

                  ;; "in degrees" / "in deg" (for inverse trig output)
                  (and (>= (count lower-remaining) 2)
                       (= "in" (first lower-remaining))
                       (#{"degrees" "deg"} (second lower-remaining)))
                  [:deg 2]

                  ;; "radians" / "rad" suffix
                  (and (>= (count lower-remaining) 1)
                       (#{"radians" "rad"} (first lower-remaining)))
                  [:rad 1]

                  ;; "degrees" / "deg" suffix
                  (and (>= (count lower-remaining) 1)
                       (#{"degrees" "deg"} (first lower-remaining)))
                  [:deg 1]

                  ;; Pi implies radians
                  pi-mode
                  [:rad 0]

                  ;; Default: degrees
                  :else
                  [:deg 0])
                ;; Check no unconsumed tokens (besides format suffixes which
                ;; are already stripped before we get here)
                leftover (subvec remaining (min consumed (count remaining)))]
            (when (empty? leftover)
              {:op :trig :fn trig-fn :value value :angle-mode angle-mode})))))))

(defn parse-modulo
  "Try to parse a modulo expression. Returns a request map or nil.
   Supports:
     'X mod Y'     → {:op :modulo :dividend X :divisor Y}
     'X modulo Y'  → same
     'what is X mod Y' → same"
  [s]
  (or
   (when-let [[_ x-str y-str] (re-matches #"(?i)^(?:what\s+is\s+)?(.+?)\s+mod(?:ulo)?\s+(.+)$" s)]
     (when-let [[x _] (parse-percentage-number x-str)]
       (when-let [[y _] (parse-percentage-number y-str)]
         {:op :modulo :dividend x :divisor y})))))

(def ^:private constant-patterns
  "Lowercase strings and Unicode chars that indicate a math constant is present."
  #{"pi" "phi" "π" "φ"})

(defn parse-standalone-math
  "Try to parse a standalone math/constant expression (no units).
   Handles 'pi', 'π', 'e', 'phi', 'φ', '2*pi', 'pi/4', 'e^2', etc.
   Only matches when the expression contains a named constant — plain
   numbers like '42' are not matched to avoid hijacking unit queries."
  [s]
  (let [lower (str/lower-case (str/trim s))]
    (when (or (some #(str/includes? lower %) constant-patterns)
              ;; 'e' is too short to substring-match; require exact or bounded
              (re-find #"(?i)(?:^e$|(?<![a-z])e(?![a-z]))" lower))
      (let [normalized (-> s (str/replace "π" "pi") (str/replace "φ" "phi"))]
        (when-let [result (parse-math normalized)]
          {:op :math-expr :value (math-value result)})))))

(defn- strip-dollar [s]
  (str/replace (str/trim s) #"^\$\s*" ""))

(defn parse-tip
  "Try to parse a tip calculation expression. Returns a request map or nil.
   Supports:
     '20% tip on $50'                → {:op :tip :percent 20 :bill 50}
     '20 percent tip on 50'          → same
     'tip on $50 at 20%'             → same
     'what is the tip on $85.50 at 18%' → {:op :tip :percent 18 :bill 85.50}
     'tip 20% on 100'                → same"
  [s]
  (or
   ;; "X% tip on Y" / "X percent tip on Y"
   (when-let [[_ pct-str bill-str] (re-matches #"(?i)^(.+?)\s+percent\s+tip\s+(?:on|for)\s+(.+)$" s)]
     (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
       (when-let [[bill _] (parse-percentage-number (strip-dollar bill-str))]
         {:op :tip :percent pct :bill bill})))

   ;; "tip X% on Y" / "tip X percent on Y"
   (when-let [[_ pct-str bill-str] (re-matches #"(?i)^(?:what\s+is\s+(?:the\s+)?)?tip\s+(.+?)\s+percent\s+(?:on|for)\s+(.+)$" s)]
     (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
       (when-let [[bill _] (parse-percentage-number (strip-dollar bill-str))]
         {:op :tip :percent pct :bill bill})))

   ;; "tip on Y at X%" / "what is the tip on Y at X%"
   (when-let [[_ bill-str pct-str] (re-matches #"(?i)^(?:what\s+is\s+(?:the\s+)?)?tip\s+(?:on|for)\s+(.+?)\s+at\s+(.+?)\s*percent$" s)]
     (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
       (when-let [[bill _] (parse-percentage-number (strip-dollar bill-str))]
         {:op :tip :percent pct :bill bill})))

   ;; "tip on Y" (default, round tip)
   (when-let [[_ bill-str] (re-matches #"(?i)^(?:what\s+is\s+(?:the\s+)?)?tip\s+(?:on|for)\s+(.+)$" s)]
     (when-let [[bill _] (parse-percentage-number (strip-dollar bill-str))]
       {:op :tip :bill bill :round-tip true}))

   ;; Brief forms: "tip X percent $Y" (percent then dollar bill)
   (when-let [[_ pct-str bill-str] (re-matches #"(?i)^tip\s+(.+?)\s+percent\s+\$(.+)$" s)]
     (when-let [[pct _] (parse-percentage-number pct-str)]
       (when-let [[bill _] (parse-percentage-number bill-str)]
         {:op :tip :percent pct :bill bill})))

   ;; Brief forms: "tip $Y X percent" or "tip N X percent" (bill then percent)
   (when-let [[_ bill-str pct-str] (re-matches #"(?i)^tip\s+\$?(.+?)\s+(.+?)\s+percent$" s)]
     (when-let [[bill _] (parse-percentage-number (strip-dollar bill-str))]
       (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
         {:op :tip :percent pct :bill bill})))

   ;; Brief form: "tip $X $Y" (two dollar amounts: bill then total)
   (when-let [[_ bill-str total-str] (re-matches #"(?i)^tip\s+\$(\S+)\s+\$(\S+)$" s)]
     (when-let [[bill _] (parse-percentage-number bill-str)]
       (when-let [[total _] (parse-percentage-number total-str)]
         (when (> total bill)
           {:op :tip :percent (* (/ (double (- total bill)) (double bill)) 100.0) :bill bill :exact true}))))

   ;; Brief form: "tip $Y N" (dollar bill then bare number percent)
   (when-let [[_ bill-str pct-str] (re-matches #"(?i)^tip\s+\$(\S+)\s+(\S+)$" s)]
     (when-let [[bill _] (parse-percentage-number bill-str)]
       (when-let [[pct _] (parse-percentage-number pct-str)]
         {:op :tip :percent pct :bill bill})))

   ;; Brief form: "tip $Y" (dollar amount, round tip)
   (when-let [[_ bill-str] (re-matches #"(?i)^tip\s+\$(\S+)$" s)]
     (when-let [[bill _] (parse-percentage-number bill-str)]
       {:op :tip :bill bill :round-tip true}))

   ;; Brief form: "tip N M" (two bare numbers: bill then percent)
   (when-let [[_ first-str second-str] (re-matches #"(?i)^tip\s+(\S+)\s+(\S+)$" s)]
     (when-let [[first-val _] (parse-percentage-number first-str)]
       (when-let [[second-val _] (parse-percentage-number second-str)]
         {:op :tip :percent second-val :bill first-val})))

   ;; Brief form: "tip N" (single bare number, round tip)
   (when-let [[_ bill-str] (re-matches #"(?i)^tip\s+(\S+)$" s)]
     (when-let [[bill _] (parse-percentage-number bill-str)]
       {:op :tip :bill bill :round-tip true}))))

(defn parse-tax
  "Try to parse a tax calculation expression. Returns a request map or nil.
   Supports:
     '10% tax on $50'               → {:op :tax :percent 10 :price 50}
     'tax on $50 at 10%'            → same
     'tax 10% $50' / 'tax $50 10%'  → same
     'tax 50 10'                    → same (price then rate)"
  [s]
  (or
   ;; "X% tax on Y" / "X percent tax on Y"
   (when-let [[_ pct-str price-str] (re-matches #"(?i)^(.+?)\s+percent\s+tax\s+(?:on|for)\s+(.+)$" s)]
     (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
       (when-let [[price _] (parse-percentage-number (strip-dollar price-str))]
         {:op :tax :percent pct :price price})))

   ;; "tax X% on Y" / "tax X percent on Y"
   (when-let [[_ pct-str price-str] (re-matches #"(?i)^(?:what\s+is\s+(?:the\s+)?)?tax\s+(.+?)\s+percent\s+(?:on|for)\s+(.+)$" s)]
     (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
       (when-let [[price _] (parse-percentage-number (strip-dollar price-str))]
         {:op :tax :percent pct :price price})))

   ;; "tax on Y at X%" / "what is the tax on Y at X%"
   (when-let [[_ price-str pct-str] (re-matches #"(?i)^(?:what\s+is\s+(?:the\s+)?)?tax\s+(?:on|for)\s+(.+?)\s+at\s+(.+?)\s*percent$" s)]
     (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
       (when-let [[price _] (parse-percentage-number (strip-dollar price-str))]
         {:op :tax :percent pct :price price})))

   ;; Brief forms: "tax X percent $Y" (percent then dollar price)
   (when-let [[_ pct-str price-str] (re-matches #"(?i)^tax\s+(.+?)\s+percent\s+\$(.+)$" s)]
     (when-let [[pct _] (parse-percentage-number pct-str)]
       (when-let [[price _] (parse-percentage-number price-str)]
         {:op :tax :percent pct :price price})))

   ;; Brief forms: "tax $Y X percent" or "tax N X percent" (price then percent)
   (when-let [[_ price-str pct-str] (re-matches #"(?i)^tax\s+\$?(.+?)\s+(.+?)\s+percent$" s)]
     (when-let [[price _] (parse-percentage-number (strip-dollar price-str))]
       (when-let [[pct _] (parse-percentage-number (strip-dollar pct-str))]
         {:op :tax :percent pct :price price})))

   ;; Brief form: "tax $X $Y" (two dollar amounts: price then total)
   (when-let [[_ price-str total-str] (re-matches #"(?i)^tax\s+\$(\S+)\s+\$(\S+)$" s)]
     (when-let [[price _] (parse-percentage-number price-str)]
       (when-let [[total _] (parse-percentage-number total-str)]
         (when (> total price)
           {:op :tax :percent (* (/ (double (- total price)) (double price)) 100.0) :price price}))))

   ;; Brief form: "tax $Y N" (dollar price then bare number rate)
   (when-let [[_ price-str pct-str] (re-matches #"(?i)^tax\s+\$(\S+)\s+(\S+)$" s)]
     (when-let [[price _] (parse-percentage-number price-str)]
       (when-let [[pct _] (parse-percentage-number pct-str)]
         {:op :tax :percent pct :price price})))

   ;; Brief form: "tax N M" (two bare numbers: price then rate)
   (when-let [[_ first-str second-str] (re-matches #"(?i)^tax\s+(\S+)\s+(\S+)$" s)]
     (when-let [[first-val _] (parse-percentage-number first-str)]
       (when-let [[second-val _] (parse-percentage-number second-str)]
         {:op :tax :percent second-val :price first-val})))))

;; ---------------------------------------------------------------------------
;; Download/upload time calculator
;; ---------------------------------------------------------------------------

(def ^:private download-size-suffixes
  "Map of case-insensitive suffix → data-size unit keyword (bytes)."
  {"b" :B "byte" :B "bytes" :B
   "k" :KB "kb" :KB "kilobyte" :KB "kilobytes" :KB
   "m" :MB "mb" :MB "megabyte" :MB "megabytes" :MB
   "g" :GB "gb" :GB "gigabyte" :GB "gigabytes" :GB
   "t" :TB "tb" :TB "terabyte" :TB "terabytes" :TB
   "p" :PB "pb" :PB "petabyte" :PB "petabytes" :PB})

(def ^:private download-rate-suffixes
  "Map of case-insensitive suffix → data-rate exponent map."
  {"bps"  {:bit 1 :s -1}
   "k"    {:Kb 1 :s -1} "kbps" {:Kb 1 :s -1}
   "m"    {:Mb 1 :s -1} "mbps" {:Mb 1 :s -1}
   "g"    {:Gb 1 :s -1} "gbps" {:Gb 1 :s -1}
   "t"    {:Tb 1 :s -1} "tbps" {:Tb 1 :s -1}
   "kbs"  {:KB 1 :s -1} "mbs"  {:MB 1 :s -1}
   "gbs"  {:GB 1 :s -1} "tbs"  {:TB 1 :s -1}})

(def ^:private rate-unit-labels
  "Display labels for rate unit exponent maps."
  {{:bit 1 :s -1} "bps"
   {:Kb 1 :s -1}  "Kbps"
   {:Mb 1 :s -1}  "Mbps"
   {:Gb 1 :s -1}  "Gbps"
   {:Tb 1 :s -1}  "Tbps"
   {:KB 1 :s -1}  "KBps"
   {:MB 1 :s -1}  "MBps"
   {:GB 1 :s -1}  "GBps"
   {:TB 1 :s -1}  "TBps"})

(defn parse-download
  "Try to parse a download/upload time expression. Returns a request map or nil.
   After clean-phrase, '10MB' becomes '10 MB' and '1g' becomes '1 g', so we
   handle both 'download NUM SUFFIX NUM SUFFIX' (4 tokens) forms.
   Supports: 'download 10MB 1000Mbps', 'upload 1g 1g', 'download 10gb 1G'"
  [s]
  (when-let [[_ size-num size-sfx rate-num rate-sfx]
             (re-matches #"(?i)^(?:download|upload)\s+(\d+(?:\.\d+)?)\s+(\S+)\s+(\d+(?:\.\d+)?)\s+(\S+)$" s)]
    (when-let [size-unit (get download-size-suffixes (str/lower-case size-sfx))]
      (when-let [rate-unit (get download-rate-suffixes (str/lower-case rate-sfx))]
        {:op :download
         :size-value (parse-decimal-token size-num)
         :size-unit size-unit
         :size-label (name size-unit)
         :rate-value (parse-decimal-token rate-num)
         :rate-unit rate-unit
         :rate-label (get rate-unit-labels rate-unit "?")}))))

;; ---------------------------------------------------------------------------
;; Base conversion (binary, octal, decimal, hex, sexagesimal, arbitrary)
;; ---------------------------------------------------------------------------

(def ^:private base-names
  {"binary" :binary "bin" :binary
   "octal" :octal "oct" :octal
   "decimal" :decimal "dec" :decimal
   "hex" :hex "hexadecimal" :hex
   "sexagesimal" :sexagesimal "sex" :sexagesimal})

(defn- parse-base-literal
  "Try to parse a base-prefixed literal or sexagesimal colon notation.
   Returns [integer-value from-base] or nil.
   Handles spaces inserted by clean-phrase (e.g. '0x ff' from '0xff')."
  [s]
  (let [s (str/trim s)]
    (cond
      ;; 0x/0X hex prefix (with optional space from clean-phrase)
      (re-matches #"(?i)^0x\s*[0-9a-f]+$" s)
      (let [digits (str/replace (str/replace s #"(?i)^0x\s*" "") #"\s+" "")]
        [(#?(:clj BigInteger. :cljs js/parseInt :lpy python/int)
          digits #?(:clj 16 :cljs 16 :lpy 16))
         :hex])

      ;; 0b/0B binary prefix (with optional space)
      (re-matches #"(?i)^0b\s*[01]+$" s)
      (let [digits (str/replace (str/replace s #"(?i)^0b\s*" "") #"\s+" "")]
        [(#?(:clj BigInteger. :cljs js/parseInt :lpy python/int)
          digits #?(:clj 2 :cljs 2 :lpy 2))
         :binary])

      ;; 0o/0O octal prefix (with optional space)
      (re-matches #"(?i)^0o\s*[0-7]+$" s)
      (let [digits (str/replace (str/replace s #"(?i)^0o\s*" "") #"\s+" "")]
        [(#?(:clj BigInteger. :cljs js/parseInt :lpy python/int)
          digits #?(:clj 8 :cljs 8 :lpy 8))
         :octal])

      ;; Sexagesimal: H:MM:SS or M:SS (colon-separated digits)
      (re-matches #"^\d+:\d{2}:\d{2}$" s)
      (let [[h m sec] (map parse-long-str (str/split s #":"))]
        [(+ (* h 3600) (* m 60) sec) :sexagesimal])

      :else nil)))

(defn- parse-base-target
  "Parse a target base name from a string. Returns a base keyword or integer, or nil.
   Handles: 'hex', 'binary', 'base 7', 'base 16', etc."
  [s]
  (let [s (str/trim (str/lower-case s))]
    (or (get base-names s)
        (when-let [[_ n] (re-matches #"base\s+(\d+)" s)]
          (let [b (parse-long-str n)]
            (when (>= b 2) b))))))

(defn- parse-value-with-base-suffix
  "Parse 'ff hex', '11111111 binary', '377 octal' — value followed by base name.
   Returns [integer-value from-base remaining-str] or nil."
  [s]
  (let [tokens (str/split (str/trim s) #"\s+")
        n (count tokens)]
    (when (>= n 2)
      (let [last-token (str/lower-case (last tokens))
            base-kw (get base-names last-token)]
        (when (and base-kw (not= base-kw :decimal) (not= base-kw :sexagesimal))
          (let [value-str (str/join " " (butlast tokens))
                radix (case base-kw :hex 16 :binary 2 :octal 8 nil)]
            (when radix
              (try
                (let [digits (str/replace value-str #"\s+" "")
                      v (#?(:clj BigInteger. :cljs js/parseInt :lpy python/int)
                         digits #?(:clj radix :cljs radix :lpy radix))]
                  [v base-kw])
                #?(:cljs (catch :default _ nil)
                   :default (catch Exception _ nil))))))))))

(defn- clean-phrase-light
  "Minimal cleaning for base conversion detection: strip ? and , collapse whitespace.
   Does NOT insert spaces between digits and letters (preserves 0xff, 0b1010, etc.)."
  [s]
  (-> s
      str/trim
      (str/replace #"[?]" "")
      (str/replace #"," "")
      (str/replace #"\s+" " ")
      str/trim))

(defn parse-base-convert
  "Try to parse a base conversion expression. Returns a request map or nil.
   Uses light cleaning to preserve base prefixes (0x, 0b, 0o).
   Supports:
     '255 in hex'              → {:op :base-convert :value 255 :from-base :decimal :to-base :hex}
     '0xff in decimal'         → {:op :base-convert :value 255 :from-base :hex :to-base :decimal}
     '0b1010 in hex'           → {:op :base-convert :value 10 :from-base :binary :to-base :hex}
     'ff hex in decimal'       → {:op :base-convert :value 255 :from-base :hex :to-base :decimal}
     '1:30:45 in decimal'      → {:op :base-convert :value 5445 :from-base :sexagesimal :to-base :decimal}
     '255 in base 7'           → {:op :base-convert :value 255 :from-base :decimal :to-base 7}"
  [_cleaned-s original-phrase]
  (let [s (clean-phrase-light original-phrase)]
    (or
     ;; With target: "X in/to Y"
     (when-let [[qty-str to-str] (split-request s)]
       (let [to-base (parse-base-target to-str)]
         (when to-base
           (or
            ;; Try base-prefixed literal on quantity side (0xff, 0b1010, 0o377, H:M:S)
            (when-let [[value from-base] (parse-base-literal qty-str)]
              {:op :base-convert :value (long value) :from-base from-base :to-base to-base})

            ;; Try base-name suffix on quantity side (ff hex, 377 octal)
            (when-let [[value from-base] (parse-value-with-base-suffix qty-str)]
              {:op :base-convert :value (long value) :from-base from-base :to-base to-base})

            ;; Plain decimal number
            (let [trimmed (str/trim qty-str)]
              (when (re-matches #"\d+" trimmed)
                {:op :base-convert
                 :value (long (#?(:clj BigInteger. :cljs js/parseInt :lpy python/int)
                               trimmed #?(:clj 10 :cljs 10 :lpy 10)))
                 :from-base :decimal
                 :to-base to-base}))

            ;; Math expression (e.g. "3*3", "2+5", "4^2")
            (when-let [v (math-value (parse-math (str/trim qty-str)))]
              (when (integer? v)
                {:op :base-convert
                 :value (long v)
                 :from-base :decimal
                 :to-base to-base}))))))

     ;; Standalone base literal (no target): "0xff", "0b1010", "1:30:45"
     ;; Default to decimal output
     (when-let [[value from-base] (parse-base-literal s)]
       {:op :base-convert :value (long value) :from-base from-base :to-base :decimal}))))

(defn parse-request [phrase]
  (let [original phrase]
    (if-let [roll (dice/parse-roll phrase)]
      roll
    (try
      (let [pre-cleaned (clean-phrase phrase)
            ;; Try full expression as standalone math first (preserves Decimal
            ;; precision for expressions like (10^100)+1-(10^100))
            [without-format-pre format-pre] (extract-format pre-cleaned)
            full-math (math-value (parse-math without-format-pre))
            cleaned (if full-math pre-cleaned (evaluate-math-exprs pre-cleaned))
            [without-format format] (extract-format cleaned)
            [without-approx approx?] (extract-approx without-format)
            base-conv (parse-base-convert without-approx original)
            tip (when-not base-conv (parse-tip without-approx))
            tax (when-not (or base-conv tip) (parse-tax without-approx))
            dl (when-not (or base-conv tip tax) (parse-download without-approx))
            pct (when-not (or base-conv tip tax dl) (parse-percentage without-approx))
            root (when-not (or base-conv tip tax dl pct) (parse-root without-approx))
            modulo (when-not (or base-conv tip tax dl pct root) (parse-modulo without-approx))
            trig (when-not (or base-conv tip tax dl pct root modulo) (parse-trig without-approx))
            math (or (when full-math {:op :math-expr :value full-math})
                     (when-not (or base-conv tip tax dl pct root modulo trig) (parse-standalone-math without-approx)))]
        (if base-conv
          (cond-> base-conv
            format (assoc :format format))
        (if tip
          (cond-> tip
            format (assoc :format format))
        (if tax
          (cond-> tax
            format (assoc :format format))
        (if dl
          (cond-> dl
            format (assoc :format format))
        (if pct
          (cond-> pct
            format (assoc :format format))
        (if root
          (cond-> root
            format (assoc :format format))
        (if modulo
          (cond-> modulo
            format (assoc :format format))
        (if trig
          (cond-> trig
            format (assoc :format format))
        (if math
          (cond-> math
            format (assoc :format format))
          (let [pieces (split-request without-approx)]
        (if-not pieces
          ;; No "in"/"to" target — try as a standalone quantity expression
          (let [qty (try (parse-quantity without-approx)
                         #?(:cljs (catch :default _ nil)
                            :default (catch Exception _ nil)))]
            (if (and qty (or (:qty-expr qty)
                             (and (map? qty) (:unit qty) (not= {} (:unit qty)))))
              (cond-> {:op :convert :quantity qty :to :auto}
                approx? (assoc :approx? true)
                format (assoc :format format))
              {:error :unparseable
               :phrase original}))
          (let [[quantity-str to-str] pieces
                ;; Try swapping if quantity side has no number
                ;; e.g. "seconds in one year" -> "one year" to "seconds"
                [quantity-str to-str] (try-swap-sides quantity-str to-str)
                ;; Check if the target side also has a quantity
                to-has-number? (try-parse-quantity to-str)]
            (if to-has-number?
              (throw (ex-info "Both sides have quantities"
                              {:error :ambiguous-quantities
                               :quantity quantity-str
                               :to to-str}))
              (let [request (cond-> {:op :convert
                                     :quantity (parse-quantity quantity-str)
                                     :to (parse-unit-phrase to-str)}
                              approx? (assoc :approx? true)
                              format (assoc :format format))]
                request)))))))))))))))
      #?(:clj (catch clojure.lang.ExceptionInfo ex
                (or (parse-error original ex)
                    {:error :unparseable
                     :phrase original}))
         :cljs (catch ExceptionInfo ex
                 (or (parse-error original ex)
                     {:error :unparseable
                      :phrase original}))
         :lpy (catch Exception ex
                (if (ex-data ex)
                  (or (parse-error original ex)
                      {:error :unparseable
                       :phrase original})
                  {:error :unparseable
                   :phrase original})))
      #?(:clj (catch Exception _
                {:error :unparseable
                 :phrase original})
         :cljs (catch :default _
                 {:error :unparseable
                  :phrase original}))))))
