(ns calc.completions
  "Context-aware completion engine for the calculator.
   Works across JVM (CLI) and ClojureScript (web)."
  (:require [clojure.string :as str]
            [calc.units :as u]
            [calc.parser :as parser]))

;; ============================================================================
;; Vocabulary — built once from unit-defs + special-unit-forms
;; ============================================================================

(def vocabulary
  "All completable unit entries: [{:text :canonical :dim :group :desc} ...]"
  (vec
   (concat
    (for [[alias-str unit-key] u/unit-aliases
          :let [info (get u/unit-defs unit-key)
                dim (if (:temperature info) :temperature (:dim info))
                group (if (:temperature info)
                        "Temperature"
                        (get u/dim-categories (:dim info) "Other"))
                short-name (get u/unit-short-names unit-key)]]
      {:text alias-str
       :canonical unit-key
       :dim dim
       :group group
       :desc (when (not= alias-str short-name) short-name)})
    (for [[alias-str unit-map] parser/special-unit-forms
          :let [dim (try (:dim (u/unit-spec unit-map))
                        (catch #?(:clj Exception :cljs :default) _ nil))
                group (if dim
                        (get u/dim-categories dim "Compound")
                        "Compound")]]
      {:text alias-str
       :canonical nil
       :dim dim
       :group group
       :desc nil}))))

(def slash-commands
  [{:text "/help"  :group "Commands" :desc "Show help"}
   {:text "/p"     :group "Commands" :desc "Set decimal precision"}
   {:text "/s"     :group "Commands" :desc "Set significant figures"}
   {:text "/clear" :group "Commands" :desc "Clear screen & history"}
   {:text "/reset" :group "Commands" :desc "Reset"}
   {:text "/roll"  :group "Commands" :desc "Roll dice"}
   {:text "/quit"  :group "Commands" :desc "Exit"}
   {:text "/exit"  :group "Commands" :desc "Exit"}])

;; ============================================================================
;; Compound dimension support
;; ============================================================================

(def ^:private dim->unit-names
  "Map from dimension-map to vector of {:text short-name :group label}.
   Used for generating compound A/B suggestions."
  (let [entries
        (concat
         ;; One entry per canonical unit using its short name
         (for [[k v] u/unit-defs
               :when (and (:dim v) (not (:temperature v)))]
           {:dim (:dim v) :text (:short v)
            :group (get u/dim-categories (:dim v) "Other")})
         ;; Special forms (mph, fps, kph, etc.)
         (for [[text unit-map] parser/special-unit-forms
               :let [dim (try (:dim (u/unit-spec unit-map))
                              (catch #?(:clj Exception :cljs :default) _ nil))]
               :when dim]
           {:dim dim :text text
            :group (get u/dim-categories dim "Compound")}))]
    (reduce (fn [m {:keys [dim] :as entry}]
              (update m dim (fnil conj []) (select-keys entry [:text :group])))
            {}
            entries)))

(defn- negate-dim
  "Negate all exponents in a dimension map."
  [dim]
  (into {} (map (fn [[k v]] [k (- v)]) dim)))

;; ============================================================================
;; Token classification helpers
;; ============================================================================

(defn- resolve-unit
  "Resolve an input word to a canonical unit keyword, or nil."
  [word]
  (or (get u/unit-aliases word)
      (get u/unit-aliases (str/lower-case word))))

(defn- token-dim
  "Get the dimension of a single unit token (simple alias or special form)."
  [token]
  (if-let [uk (resolve-unit token)]
    (let [info (get u/unit-defs uk)]
      (when-not (:temperature info) (:dim info)))
    (when-let [unit-map (or (get parser/special-unit-forms token)
                            (get parser/special-unit-forms (str/lower-case token)))]
      (try (:dim (u/unit-spec unit-map))
           (catch #?(:clj Exception :cljs :default) _ nil)))))

(defn- compound-dim
  "Parse a potentially compound token like 'mph/gram' and return its dimension.
   Handles A/B/C by treating each / as division."
  [token]
  (if (str/includes? token "/")
    (let [parts (str/split token #"/")
          dims (map token-dim parts)]
      (when (every? some? dims)
        (u/normalize-map
         (reduce (fn [acc d] (u/merge-dims acc (negate-dim d)))
                 (first dims)
                 (rest dims)))))
    ;; Also check temperature for simple tokens
    (or (token-dim token)
        (when-let [uk (resolve-unit token)]
          (when (:temperature (get u/unit-defs uk))
            :temperature)))))

(defn- number-token? [s]
  (boolean (re-matches #"-?\d[\d,]*\.?\d*(?:/\d+)?" s)))

(defn- unit-token? [s]
  (boolean (or (resolve-unit s)
               (get parser/special-unit-forms s)
               (get parser/special-unit-forms (str/lower-case s))
               (and (str/includes? s "/")
                    (some? (compound-dim s))))))

(defn- connector-token? [s]
  (contains? #{"in" "to"} (str/lower-case s)))

(defn- prefix-match? [prefix text]
  (str/starts-with? (str/lower-case text) (str/lower-case prefix)))

;; ============================================================================
;; Context detection
;; ============================================================================

(defn- find-source-dim
  "Walk prior words backwards to find a unit (including compounds) and return its dimension."
  [words]
  (some compound-dim (reverse words)))

;; ============================================================================
;; Compound suggestion generation
;; ============================================================================

(defn- generate-compound-suggestions
  "Generate A/B compound unit suggestions that match target-dim.
   For each denominator unit B, compute needed numerator dim = target-dim + dim(B),
   then pair with all numerator units that have that dim."
  [target-dim prefix]
  (when (map? target-dim)
    (let [results
          (for [[denom-dim denom-units] dim->unit-names
                :let [numer-dim (u/merge-dims target-dim denom-dim)]
                :when (seq numer-dim)
                :when (not= numer-dim target-dim)  ;; actually dividing by something
                :when (contains? dim->unit-names numer-dim)
                numer (get dim->unit-names numer-dim)
                denom denom-units
                :let [text (str (:text numer) "/" (:text denom))]
                :when (or (str/blank? prefix)
                          (prefix-match? prefix text))]
            {:text text :group "Compound" :desc nil})]
      (->> results distinct (sort-by :text) vec))))

(defn- complete-compound-denom
  "Complete the denominator of a compound prefix like 'fps/p'.
   If target-dim is known, filter denominators to produce matching compounds."
  [numer denom-prefix target-dim]
  (let [numer-dim (token-dim numer)
        ;; If we know the target dim, compute what denominator dim is needed:
        ;; target = numer/denom → denom-dim = numer-dim - target-dim
        needed-denom-dim (when (and numer-dim target-dim (map? target-dim))
                           (u/normalize-map
                            (u/merge-dims numer-dim (negate-dim target-dim))))
        candidates (if needed-denom-dim
                     ;; Dimension-filtered: only units that produce the right compound dim
                     (filter #(= needed-denom-dim (:dim %)) vocabulary)
                     vocabulary)]
    (->> candidates
         (filter #(or (str/blank? denom-prefix)
                      (prefix-match? denom-prefix (:text %))))
         (map #(assoc % :text (str numer "/" (:text %))
                        :group "Compound"))
         (sort-by :text)
         vec)))

;; ============================================================================
;; Main completion function
;; ============================================================================

(defn complete
  "Return completions for the given input buffer.
   Each result: {:text string :group string :desc string-or-nil}"
  [buffer]
  (let [buf (or buffer "")
        trimmed (str/trim buf)
        at-space? (and (seq buf) (= \space (last buf)))
        parts (if (str/blank? trimmed) [] (str/split trimmed #"\s+"))
        [prior prefix] (if at-space?
                         [parts ""]
                         [(vec (butlast parts)) (or (last parts) "")])
        filter-prefix (fn [entries]
                        (if (str/blank? prefix)
                          entries
                          (filter #(prefix-match? prefix (:text %)) entries)))]
    (cond
      ;; Slash commands
      (and (seq prefix) (str/starts-with? prefix "/"))
      (vec (filter-prefix slash-commands))

      ;; Compound prefix: "fps/p" → complete denominator
      (and (seq prefix) (str/includes? prefix "/"))
      (let [slash-idx (str/index-of prefix "/")
            numer (subs prefix 0 slash-idx)
            denom-prefix (subs prefix (inc slash-idx))
            ;; Determine target dim if we're after a connector
            target-dim (when (and (seq prior) (connector-token? (last prior)))
                         (find-source-dim (butlast prior)))]
        (when (seq denom-prefix)  ;; only suggest after at least 1 char of denominator
          (complete-compound-denom numer denom-prefix target-dim)))

      ;; After "in"/"to" → suggest target units, dimension-filtered
      (and (seq prior) (connector-token? (last prior)))
      (let [src-dim (find-source-dim (butlast prior))
            simple (if src-dim
                     (->> vocabulary
                          (filter #(= src-dim (:dim %)))
                          filter-prefix
                          (sort-by :text)
                          vec)
                     (->> vocabulary filter-prefix (sort-by :text) vec))
            ;; Generate compound suggestions when no simple vocab matches
            compounds (when (and src-dim (empty? simple))
                        (generate-compound-suggestions src-dim prefix))]
        (vec (concat simple compounds)))

      ;; After a unit (including compounds like "mph/gram") → suggest connectors
      (and (seq prior) (unit-token? (last prior)))
      (vec (filter-prefix [{:text "in" :group "Connector" :desc nil}
                           {:text "to" :group "Connector" :desc nil}]))

      ;; After a number → suggest all units
      (and (seq prior) (number-token? (last prior)))
      (->> vocabulary filter-prefix (sort-by :text) vec)

      ;; Typing with a non-empty prefix → suggest matching units
      (seq prefix)
      (->> vocabulary filter-prefix (sort-by :text) vec)

      ;; Empty input / no context → nothing
      :else [])))
