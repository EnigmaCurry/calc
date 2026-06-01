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

(defn- preferred-alias
  "Pick the shortest alias that is recognized by the parser for a unit."
  [unit-key]
  (let [{:keys [aliases short]} (get u/unit-defs unit-key)
        ;; Prefer short name if it's a valid alias, otherwise pick shortest alias
        valid-aliases (filter #(get u/unit-aliases %) aliases)]
    (if (and short (get u/unit-aliases short))
      short
      (first (sort-by count valid-aliases)))))

(def ^:private unique-special-forms
  "Deduplicated special forms: keep only the shortest name per unique unit-map.
   E.g., kph/kmph/km/h all map to {:km 1 :hr -1} — keep 'kph'."
  (let [by-unit-map (group-by val parser/special-unit-forms)]
    (into {} (for [[unit-map entries] by-unit-map
                   :let [shortest (first (sort-by count (map key entries)))]]
               [shortest unit-map]))))

(def ^:private dim->unit-names
  "Map from dimension-map to vector of {:text alias :group label}.
   Only includes names verified to resolve through the parser.
   Deduplicated: one entry per canonical unit or unique special form."
  (let [entries
        (concat
         ;; One entry per canonical unit using its shortest valid alias
         (for [[k v] u/unit-defs
               :when (and (:dim v) (not (:temperature v)))
               :let [alias (preferred-alias k)]
               :when alias]
           {:dim (:dim v) :text alias
            :group (get u/dim-categories (:dim v) "Other")})
         ;; Deduplicated special forms (mph, fps, kph — not kmph, km/h, ft/s)
         (for [[text unit-map] unique-special-forms
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

(defn- format-raw-dim
  "Format a dimension map as raw component labels, e.g. {:length 1 :time 2} → 'Length · Time^2'."
  [d]
  (->> (sort-by key d)
       (map (fn [[k v]]
              (let [base (get u/dim-categories {k 1}
                              (str/capitalize (name k)))
                    av (if (pos? v) v (- v))]
                (if (> av 1) (str base "^" av) base))))
       (str/join " · ")))

(defn- factor-dim
  "Iteratively factor a dimension map into known categories.
   At each step, picks the move (numer subtract or denom add) that
   reduces the most dimension entries. Returns [numer-names denom-names remaining]."
  [dim]
  (loop [remaining dim, numers [], denoms [], iters 5]
    (if (or (empty? remaining) (zero? iters))
      [numers denoms remaining]
      (let [moves
            (concat
             ;; Numerator: subtract category (exponents must fit, must have positive components)
             (for [[cat-dim cat-name] u/dim-categories
                   :when (some (fn [[_ v]] (pos? v)) cat-dim) ;; skip pure-negative (e.g. Frequency)
                   :let [r (u/normalize-map (u/merge-dims remaining (negate-dim cat-dim)))]
                   :when (< (count r) (count remaining))
                   :when (every? (fn [[k v]]
                                   (if (pos? v)
                                     (<= v (get remaining k 0))
                                     (>= v (get remaining k 0))))
                                 cat-dim)]
               {:type :numer :name cat-name :remainder r
                :reduction (- (count remaining) (count r))})
             ;; Denominator: add category (simplicity check only)
             (for [[cat-dim cat-name] u/dim-categories
                   :let [r (u/normalize-map (u/merge-dims remaining cat-dim))]
                   :when (< (count r) (count remaining))]
               {:type :denom :name cat-name :remainder r
                :reduction (- (count remaining) (count r))}))
            ;; Prefer numer over denom when reduction ties (more natural factoring)
            best (last (sort-by (juxt :reduction
                                      (fn [m] (if (= :numer (:type m)) 1 0)))
                                moves))]
        (if best
          (recur (:remainder best)
                 (if (= :numer (:type best)) (conj numers (:name best)) numers)
                 (if (= :denom (:type best)) (conj denoms (:name best)) denoms)
                 (dec iters))
          [numers denoms remaining])))))

(defn dim-label
  "Human-readable label for a dimension map.
   Iteratively factors into known categories:
     {:length 1 :time -1 :mass -1} → 'Speed / Mass'
     {:data 1 :time 2 :mass -1 :length -2 :current 1} → 'Data / Time · Electrical Potential'"
  [dim]
  (when (map? dim)
    (or
     (get u/dim-categories dim)
     (let [[numers denoms remaining] (factor-dim dim)]
       (when (or (seq numers) (seq denoms))
         ;; Incorporate any unfactored remainder by sign
         (let [rem-pos (into {} (filter (fn [[_ v]] (pos? v)) remaining))
               rem-neg (into {} (filter (fn [[_ v]] (neg? v)) remaining))
               numer-parts (concat numers
                                   (when (seq rem-pos) [(format-raw-dim rem-pos)]))
               denom-parts (concat denoms
                                   (when (seq rem-neg) [(format-raw-dim (negate-dim rem-neg))]))
               numer-str (when (seq numer-parts) (str/join " · " numer-parts))
               denom-str (when (seq denom-parts) (str/join " · " denom-parts))]
           (cond
             (and numer-str denom-str) (str numer-str " / " denom-str)
             numer-str numer-str
             :else nil))))
     ;; Raw fallback for unfactorable dims
     (let [pos (into {} (filter (fn [[_ v]] (pos? v)) dim))
           neg (into {} (filter (fn [[_ v]] (neg? v)) dim))]
       (if (seq neg)
         (str (format-raw-dim pos) " / " (format-raw-dim (negate-dim neg)))
         (format-raw-dim pos))))))

;; ============================================================================
;; Token classification helpers
;; ============================================================================

(defn- resolve-unit
  "Resolve an input word to a canonical unit keyword, or nil."
  [word]
  (or (get u/unit-aliases word)
      (get u/unit-aliases (str/lower-case word))))

(defn- strip-number-prefix
  "Strip a leading numeric prefix from a token: '12ft' → 'ft', '3.5kg' → 'kg'.
   Returns the unit part, or the original token if no prefix found."
  [token]
  (let [m (re-find #"^-?[\d,]*\.?\d+(.+)$" token)]
    (if m (second m) token)))

(defn- parse-unit-exponent
  "Parse 'unit^N' → [unit-name N], or [token 1] if no exponent."
  [token]
  (if-let [[_ unit exp-str] (re-matches #"(.+)\^(-?\d+)" token)]
    [unit #?(:clj (Long/parseLong exp-str) :cljs (js/parseInt exp-str 10))]
    [token 1]))

(defn- token-dim
  "Get the dimension of a single unit token (simple alias or special form).
   Handles abutted number+unit tokens like '12ft' and exponents like 'inch^3'."
  [token]
  (let [try-resolve (fn [t]
                      (or (when-let [uk (resolve-unit t)]
                            (let [info (get u/unit-defs uk)]
                              (when-not (:temperature info) (:dim info))))
                          (when-let [unit-map (or (get parser/special-unit-forms t)
                                                  (get parser/special-unit-forms (str/lower-case t)))]
                            (try (:dim (u/unit-spec unit-map))
                                 (catch #?(:clj Exception :cljs :default) _ nil)))))
        stripped (strip-number-prefix token)
        [base-unit exp] (parse-unit-exponent stripped)]
    (or (try-resolve token)
        (when (not= stripped token)
          (try-resolve stripped))
        ;; Handle unit^N (e.g., inch^3, meter^2)
        (when (not= exp 1)
          (when-let [base-dim (try-resolve base-unit)]
            (u/normalize-map (u/scale-dim base-dim exp)))))))

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
  (let [t (strip-number-prefix s)]
    (boolean (or (resolve-unit t)
                 (get parser/special-unit-forms t)
                 (get parser/special-unit-forms (str/lower-case t))
                 (and (str/includes? t "/")
                      (some? (compound-dim t)))))))

(defn- connector-token? [s]
  (contains? #{"in" "to"} (str/lower-case s)))

(defn- prefix-match? [prefix text]
  (str/starts-with? (str/lower-case text) (str/lower-case prefix)))

;; ============================================================================
;; Context detection
;; ============================================================================

(defn- join-slash-compounds
  "Reconstruct compound expressions from words separated by '/'.
   ['MBps' '/' 'V'] → ['MBps/V']
   ['12' 'MBps' '/' 'V'] → ['12' 'MBps/V']"
  [words]
  (loop [ws words, result []]
    (if (empty? ws)
      result
      (if (and (>= (count ws) 3)
               (= "/" (second ws))
               (not (number-token? (first ws))))
        ;; Join word/word and continue (handles A / B / C chains)
        (recur (cons (str (first ws) "/" (nth ws 2)) (drop 3 ws))
               result)
        (recur (rest ws) (conj result (first ws)))))))

(defn- find-source-dim
  "Walk prior words backwards to find a unit (including compounds) and return its dimension.
   Joins space-separated '/' expressions first: 'MBps / V' → 'MBps/V'."
  [words]
  (some compound-dim (reverse (join-slash-compounds words))))

;; ============================================================================
;; Magnitude-aware sorting
;; ============================================================================

(defn- to-double [x]
  #?(:clj (double x) :cljs x))

(defn- token-si-scale
  "Get the SI scale factor as a double for a unit token. Returns nil for unresolvable."
  [token]
  (try
    (or
     (when-let [uk (resolve-unit token)]
       (when-let [info (get u/unit-defs uk)]
         (when-not (:temperature info)
           (to-double (:scale info)))))
     (when-let [um (or (get parser/special-unit-forms token)
                        (get parser/special-unit-forms (str/lower-case token)))]
       (to-double (:scale (u/unit-spec um))))
     (let [[base exp] (parse-unit-exponent token)]
       (when (and (not= exp 1) (resolve-unit base))
         (to-double (:scale (u/unit-spec {(resolve-unit base) exp})))))
     (let [stripped (strip-number-prefix token)]
       (when (not= stripped token)
         (token-si-scale stripped))))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn- compound-si-scale
  "Get SI scale for a potentially compound token like 'mph/gram'."
  [token]
  (if (str/includes? token "/")
    (let [parts (str/split token #"/")
          scales (map token-si-scale parts)]
      (when (every? some? scales)
        (reduce / scales)))
    (token-si-scale token)))

(defn- parse-double [s]
  (try
    (#?(:clj Double/parseDouble :cljs js/parseFloat) (str/replace s "," ""))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn- source-si-value
  "Compute |source_value * source_scale| in SI base units as a double.
   Scans words backwards for a number+unit pair."
  [words]
  (let [joined (join-slash-compounds words)]
    (loop [i (dec (count joined))]
      (when (>= i 0)
        (let [w (nth joined i)
              stripped (strip-number-prefix w)
              result
              (cond
                ;; Abutted: "12ft"
                (and (not= stripped w) (compound-si-scale stripped))
                (when-let [v (parse-double (subs w 0 (- (count w) (count stripped))))]
                  (Math/abs (* v (compound-si-scale stripped))))

                ;; Standalone unit with preceding number
                (and (compound-si-scale w) (> i 0) (number-token? (nth joined (dec i))))
                (when-let [v (parse-double (nth joined (dec i)))]
                  (Math/abs (* v (compound-si-scale w)))))]
          (or result (recur (dec i))))))))

(defn- sort-by-closeness
  "Sort candidates by how close source_si / target_scale is to 1.0.
   Falls back to alphabetical for candidates without computable scale."
  [source-si candidates]
  (if source-si
    (let [scored (map (fn [entry]
                        (let [scale (or
                                     ;; Simple unit from vocabulary
                                     (when-let [uk (:canonical entry)]
                                       (when-let [info (get u/unit-defs uk)]
                                         (when-not (:temperature info)
                                           (to-double (:scale info)))))
                                     ;; Compound or special form: compute from text
                                     (compound-si-scale (:text entry)))
                              closeness (if scale
                                          (Math/abs (#?(:clj Math/log10 :cljs js/Math.log10)
                                                     (/ source-si scale)))
                                          1e9)]
                          (assoc entry :closeness closeness)))
                      candidates)]
      (vec (map #(dissoc % :closeness) (sort-by :closeness scored))))
    (vec (sort-by :text candidates))))

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
                :let [text (str (:text numer) "/" (:text denom))
                      group (str (:group numer) " / " (:group denom))]
                :when (or (str/blank? prefix)
                          (prefix-match? prefix text))]
            {:text text :group group :desc nil})]
      (->> results distinct (sort-by :text) vec))))

(defn- complete-compound-denom
  "Complete the denominator of a compound prefix like 'fps/p'.
   If target-dim is known, filter denominators to produce matching compounds."
  [numer denom-prefix target-dim]
  (let [numer-dim (token-dim numer)
        numer-group (when numer-dim
                      (get u/dim-categories numer-dim "Other"))
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
                        :group (str (or numer-group "Compound")
                                    " / " (:group %))))
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

      ;; After "in"/"to" → suggest target units, dimension-filtered, magnitude-sorted
      (and (seq prior) (connector-token? (last prior)))
      (let [before-connector (butlast prior)
            src-dim (find-source-dim before-connector)
            source-si (source-si-value before-connector)
            simple (if src-dim
                     (->> vocabulary
                          (filter #(= src-dim (:dim %)))
                          filter-prefix
                          (sort-by-closeness source-si))
                     (->> vocabulary filter-prefix (sort-by :text) vec))
            ;; Generate compound suggestions when no simple vocab matches
            compounds (when (and src-dim (empty? simple))
                        (sort-by-closeness source-si
                          (generate-compound-suggestions src-dim prefix)))]
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

;; ============================================================================
;; Dimension hint for preview
;; ============================================================================

(defn target-dim-hint
  "When the buffer is waiting for a target unit (ends with 'in'/'to' + optional prefix),
   return a human-readable label for the expected dimension, or nil."
  [buffer]
  (let [buf (or buffer "")
        trimmed (str/trim buf)
        at-space? (and (seq buf) (= \space (last buf)))
        parts (if (str/blank? trimmed) [] (str/split trimmed #"\s+"))]
    (when (>= (count parts) 2)
      (let [[prior _prefix] (if at-space?
                              [parts nil]
                              [(vec (butlast parts)) (last parts)])
            ;; Find the connector position
            connector-idx (some (fn [i]
                                  (when (connector-token? (get prior i)) i))
                                (range (dec (count prior)) -1 -1))]
        (when connector-idx
          (let [before-connector (subvec prior 0 connector-idx)
                src-dim (find-source-dim before-connector)]
            (dim-label src-dim)))))))
