(ns calc.dice
  (:require [clojure.string :as str]))

;; ============================================================================
;; Parser
;; ============================================================================

(defn parse-dice
  "Parse a dice expression string into a structured map.
   Returns {:count N :sides S :modifier M :keep-drop {:op :kh/:kl/:dh/:dl :n K}
            :explode bool :comparison {:op :>=/:>/:<=/:</:== :target T}}
   or {:error ... :message ...} on invalid input."
  [expr]
  (let [s (str/trim expr)
        ;; Match: [count]d<sides>[!][kh/kl/dh/dl N][+/-M][>=/<=/>/</== T]
        pattern #"(?i)^(\d+)?\s*d\s*(\d+)\s*(!?)\s*(?:(kh|kl|dh|dl)\s*(\d+))?\s*(?:([+-])\s*(\d+))?\s*(?:(>=|<=|>|<|==)\s*(\d+))?\s*$"
        m (re-matches pattern s)]
    (if-not m
      {:error :invalid-syntax :message (str "Cannot parse dice expression: " expr)}
      (let [[_ count-str sides-str bang-str
             kd-op-str kd-n-str
             mod-sign mod-val-str
             cmp-op-str cmp-target-str] m
            dice-count (if (str/blank? count-str) 1
                           #?(:clj (Long/parseLong count-str)
                              :cljs (js/parseInt count-str 10)))
            sides (if (str/blank? sides-str) 0
                      #?(:clj (Long/parseLong sides-str)
                         :cljs (js/parseInt sides-str 10)))
            explode (= "!" bang-str)
            kd-op (when kd-op-str (keyword (str/lower-case kd-op-str)))
            kd-n (when kd-n-str
                   #?(:clj (Long/parseLong kd-n-str)
                      :cljs (js/parseInt kd-n-str 10)))
            modifier (if mod-val-str
                       (let [v #?(:clj (Long/parseLong mod-val-str)
                                  :cljs (js/parseInt mod-val-str 10))]
                         (if (= "-" mod-sign) (- v) v))
                       0)
            cmp-op (when cmp-op-str (keyword cmp-op-str))
            cmp-target (when cmp-target-str
                         #?(:clj (Long/parseLong cmp-target-str)
                            :cljs (js/parseInt cmp-target-str 10)))]
        (cond
          (< dice-count 1)
          {:error :invalid-count :message "Dice count must be at least 1."}

          (< sides 2)
          {:error :invalid-sides :message "Sides must be at least 2."}

          (> dice-count 1000)
          {:error :invalid-count :message "Dice count cannot exceed 1000."}

          (> sides 1000000)
          {:error :invalid_sides :message "Sides cannot exceed 1000000."}

          (and kd-n (<= kd-n 0))
          {:error :invalid-keep-count :message "Keep/drop count must be at least 1."}

          (and kd-n (#{:kh :kl} kd-op) (> kd-n dice-count))
          {:error :invalid-keep-count
           :message (str "Cannot keep " kd-n " dice from a " dice-count "d" sides " roll.")}

          (and kd-n (#{:dh :dl} kd-op) (>= kd-n dice-count))
          {:error :invalid-keep-count
           :message (str "Cannot drop " kd-n " dice from a " dice-count "d" sides " roll.")}

          :else
          (cond-> {:count dice-count :sides sides :modifier modifier :explode explode}
            kd-op (assoc :keep-drop {:op kd-op :n kd-n})
            cmp-op (assoc :comparison {:op cmp-op :target cmp-target})))))))

;; ============================================================================
;; Roller
;; ============================================================================

(def ^:private max-explosions 100)

(defn- roll-one
  "Roll a single die with the given number of sides. Returns an integer 1..sides."
  [sides]
  (inc (rand-int sides)))

(defn- roll-exploding-die
  "Roll a single exploding die. Returns {:initial N :exploded [N ...] :total T}."
  [sides]
  (let [initial (roll-one sides)]
    (if (not= initial sides)
      {:initial initial :exploded [] :total initial}
      (loop [explosions [initial]
             n 0]
        (if (>= n max-explosions)
          {:initial initial :exploded (vec (rest explosions)) :total (reduce + explosions)}
          (let [r (roll-one sides)]
            (if (= r sides)
              (recur (conj explosions r) (inc n))
              (let [all (conj explosions r)]
                {:initial initial :exploded (vec (rest all)) :total (reduce + all)}))))))))

(defn- apply-keep-drop
  "Apply keep/drop to a sequence of values. Returns {:kept [...] :dropped [...]}."
  [{:keys [op n]} values]
  (let [sorted (sort values)
        rsorted (reverse sorted)]
    (case op
      :kh (let [kept (vec (take n rsorted))
                dropped (vec (drop n rsorted))]
            {:kept kept :dropped dropped})
      :kl (let [kept (vec (take n sorted))
                dropped (vec (drop n sorted))]
            {:kept kept :dropped dropped})
      :dh (let [dropped (vec (take n rsorted))
                kept (vec (drop n rsorted))]
            {:kept kept :dropped dropped})
      :dl (let [dropped (vec (take n sorted))
                kept (vec (drop n sorted))]
            {:kept kept :dropped dropped}))))

(defn- compare-result
  "Evaluate a comparison. Returns {:op ... :target ... :success bool}."
  [{:keys [op target]} total]
  (let [result (case op
                 :>= (>= total target)
                 :>  (> total target)
                 :<= (<= total target)
                 :<  (< total target)
                 :== (== total target))]
    {:op op :target target :success result}))

(defn roll
  "Roll dice according to a parsed dice expression. Returns a result map."
  [{:keys [count sides modifier explode keep-drop comparison] :as parsed}]
  (when (:error parsed)
    (throw (ex-info (:message parsed) parsed)))
  (if explode
    ;; Exploding dice
    (let [rolls (vec (repeatedly count #(roll-exploding-die sides)))
          values (map :total rolls)
          {kept :kept dropped :dropped}
          (if keep-drop
            (apply-keep-drop keep-drop (vec values))
            {:kept (vec values) :dropped []})
          subtotal (reduce + kept)
          total (+ subtotal modifier)]
      (cond-> {:rolls rolls
               :kept kept
               :dropped dropped
               :modifier modifier
               :total total}
        comparison (assoc :comparison (compare-result comparison total))))
    ;; Normal dice
    (let [rolls (vec (repeatedly count #(roll-one sides)))
          {kept :kept dropped :dropped}
          (if keep-drop
            (apply-keep-drop keep-drop rolls)
            {:kept rolls :dropped []})
          subtotal (reduce + kept)
          total (+ subtotal modifier)]
      (cond-> {:rolls rolls
               :kept kept
               :dropped dropped
               :modifier modifier
               :total total}
        comparison (assoc :comparison (compare-result comparison total))))))

;; ============================================================================
;; Roll command parsing ("roll ...")
;; ============================================================================

(defn roll-input?
  "Returns the dice expression string if input starts with 'roll ', else nil."
  [input]
  (let [s (str/trim input)
        lower (str/lower-case s)]
    (when (str/starts-with? lower "roll ")
      (str/trim (subs s 5)))))

(defn parse-roll
  "If input starts with 'roll ', parse the dice expression.
   Returns {:op :roll :dice <parsed> :input <original>} or nil."
  [input]
  (when-let [expr (roll-input? input)]
    (let [parsed (parse-dice expr)]
      {:op :roll :dice parsed :input expr})))
