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
;; Token classification helpers
;; ============================================================================

(defn- resolve-unit
  "Resolve an input word to a canonical unit keyword, or nil."
  [word]
  (or (get u/unit-aliases word)
      (get u/unit-aliases (str/lower-case word))))

(defn- unit-dim
  "Get the dimension of a word interpreted as a unit, or nil."
  [word]
  (if-let [uk (resolve-unit word)]
    (let [info (get u/unit-defs uk)]
      (if (:temperature info) :temperature (:dim info)))
    (when-let [unit-map (or (get parser/special-unit-forms word)
                            (get parser/special-unit-forms (str/lower-case word)))]
      (try (:dim (u/unit-spec unit-map))
           (catch #?(:clj Exception :cljs :default) _ nil)))))

(defn- number-token? [s]
  (boolean (re-matches #"-?\d[\d,]*\.?\d*(?:/\d+)?" s)))

(defn- unit-token? [s]
  (boolean (or (resolve-unit s)
               (get parser/special-unit-forms s)
               (get parser/special-unit-forms (str/lower-case s)))))

(defn- connector-token? [s]
  (contains? #{"in" "to"} (str/lower-case s)))

(defn- prefix-match? [prefix text]
  (str/starts-with? (str/lower-case text) (str/lower-case prefix)))

;; ============================================================================
;; Context detection
;; ============================================================================

(defn- find-source-dim
  "Walk prior words backwards to find a unit and return its dimension."
  [words]
  (some unit-dim (reverse words)))

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

      ;; After "in"/"to" → suggest target units, dimension-filtered
      (and (seq prior) (connector-token? (last prior)))
      (let [src-dim (find-source-dim (butlast prior))
            filtered (if src-dim
                       (filter #(= src-dim (:dim %)) vocabulary)
                       vocabulary)]
        (->> filtered filter-prefix (sort-by :text) vec))

      ;; After a unit → suggest connectors
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
