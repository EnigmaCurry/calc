(ns calc.web
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [calc.units :as units]
            [calc.math :as m]
            [calc.eval :as ev]
            [calc.format :as fmt]
            [calc.parser :as parser]
            [calc.completions :as completions]
            [clojure.string :as str]))

(defn multiline-result
  "Render a multiline result as individual divs so wrap indicators work per-line."
  [s css-class]
  (let [lines (str/split (str s) #"\n")]
    (into [:div {:class css-class}]
          (map-indexed (fn [i line] [:div.result-line {:key i} line]) lines))))

(defn format-unit-label
  "Format an exponent-map unit like {:ft 2} as 'ft²'."
  [unit]
  (cond
    (keyword? unit)
    (get units/unit-short-names unit (name unit))

    (map? unit)
    (let [pos (into {} (filter (fn [[_ v]] (pos? v))) unit)
          neg (into {} (filter (fn [[_ v]] (neg? v))) unit)]
      (str (str/join "·" (for [[k v] pos]
                           (let [label (get units/unit-short-names k (name k))]
                             (if (= v 1) label (str label "^" v)))))
           (when (seq neg)
             (str "/" (str/join "·" (for [[k v] neg]
                                     (let [label (get units/unit-short-names k (name k))
                                           exp (- v)]
                                       (if (= exp 1) label (str label "^" exp)))))))))

    :else (str unit)))

(defn format-quantity-label
  "Build a canonical display string like '2 m/s' from a parsed quantity."
  [quantity]
  (cond
    (map? quantity)
    (let [val-str (fmt/format-number (:value quantity) nil)]
      (str val-str " " (format-unit-label (:unit quantity))))

    (vector? quantity)
    (str/join " " (map format-quantity-label quantity))

    :else (str quantity)))

(defn evaluate [input fmt-opts]
  (let [input (str/trim input)]
    (when-not (str/blank? input)
      (try
        ;; First try as pure math expression
        (if-let [math-result (parser/parse-math input)]
          (if (map? math-result)
            {:result (str (:trig-expr math-result) " = "
                          (fmt/format-number (:math-value math-result) fmt-opts))}
            {:result (fmt/format-number math-result (assoc fmt-opts :original-expr input))})
          ;; Then try as unit conversion
          (let [parsed (parser/parse-request input)
                effective-fmt (merge (:format parsed) fmt-opts)
                result (ev/convert-request parsed)]
            (cond
              (not (:ok? result))
              {:error (fmt/format-error result)}

              (fmt/format-op-result parsed result effective-fmt)
              {:result (fmt/format-op-result parsed result effective-fmt)}

              ;; Mixed output (e.g., "feet and inches")
              (:mixed result)
              {:from input
               :result (str/join " "
                                 (for [{:keys [value unit-label]} (:mixed result)]
                                   (str (fmt/format-number value effective-fmt) " " unit-label)))}

              (:unit-label result)
              {:from input
               :result (str (fmt/format-number (:value result) effective-fmt) " " (:unit-label result))}

              :else
              (let [display (parser/split-display-parts input)
                    from (or (:from display) (format-quantity-label (:quantity parsed)))
                    target (when (not= :auto (:to parsed))
                             (or (:target display) (format-unit-label (:to parsed))))]
                (if target
                  {:from from
                   :target target
                   :result (fmt/format-number (:value result) effective-fmt)}
                  {:result (fmt/format-number (:value result) effective-fmt)})))))
        (catch :default e
          {:error (if-let [data (.-data e)]
                    (fmt/format-error (js->clj data :keywordize-keys true))
                    (.-message e))})))))


(defn load-history []
  (try
    (when-let [raw (.getItem js/localStorage "calc-history")]
      (js->clj (js/JSON.parse raw) :keywordize-keys true))
    (catch :default _ [])))

(defn save-history! [history]
  (try
    (.setItem js/localStorage "calc-history"
              (js/JSON.stringify (clj->js history)))
    (catch :default _ nil)))

;;; ---------- Color utilities ----------

(defn hsl->rgb
  "Convert HSL [0-360, 0-100, 0-100] to RGB [0-255, 0-255, 0-255]."
  [[h s l]]
  (let [s (/ s 100) l (/ l 100)
        c (* (- 1 (js/Math.abs (- (* 2 l) 1))) s)
        x (* c (- 1 (js/Math.abs (- (mod (/ h 60) 2) 1))))
        m (- l (/ c 2))
        [r1 g1 b1] (cond
                      (< h 60)  [c x 0]
                      (< h 120) [x c 0]
                      (< h 180) [0 c x]
                      (< h 240) [0 x c]
                      (< h 300) [x 0 c]
                      :else     [c 0 x])]
    [(js/Math.round (* (+ r1 m) 255))
     (js/Math.round (* (+ g1 m) 255))
     (js/Math.round (* (+ b1 m) 255))]))

(defn rgb->hsl
  "Convert RGB [0-255, 0-255, 0-255] to HSL [0-360, 0-100, 0-100]."
  [[r g b]]
  (let [r (/ r 255) g (/ g 255) b (/ b 255)
        mx (max r g b) mn (min r g b)
        l (/ (+ mx mn) 2)
        d (- mx mn)]
    (if (zero? d)
      [0 0 (js/Math.round (* l 100))]
      (let [s (if (> l 0.5) (/ d (- 2 mx mn)) (/ d (+ mx mn)))
            h (cond
                (= mx r) (* 60 (mod (/ (- g b) d) 6))
                (= mx g) (* 60 (+ (/ (- b r) d) 2))
                :else     (* 60 (+ (/ (- r g) d) 4)))
            h (if (neg? h) (+ h 360) h)]
        [(js/Math.round h)
         (js/Math.round (* s 100))
         (js/Math.round (* l 100))]))))

(defn hex->hsl [hex]
  (let [hex (if (str/starts-with? hex "#") (subs hex 1) hex)
        r (js/parseInt (subs hex 0 2) 16)
        g (js/parseInt (subs hex 2 4) 16)
        b (js/parseInt (subs hex 4 6) 16)]
    (rgb->hsl [r g b])))

(defn hsl->hex [[h s l]]
  (let [[r g b] (hsl->rgb [h s l])]
    (str "#"
         (.padStart (.toString r 16) 2 "0")
         (.padStart (.toString g 16) 2 "0")
         (.padStart (.toString b 16) 2 "0"))))

(defn clamp [v lo hi] (min hi (max lo v)))

(defn derive-theme
  "Given bg and accent as [h s l], derive all CSS variable values."
  [[bh bs bl :as bg] [ah as al :as accent]]
  (let [dark? (< bl 50)]
    {:bg (hsl->hex bg)
     :surface (hsl->hex [bh bs (clamp (if dark? (+ bl 5) (- bl 4)) 0 100)])
     :border (hsl->hex [bh (clamp (if dark? (max bs 10) bs) 0 100)
                         (clamp (if dark? (+ bl 15) (- bl 20)) 0 100)])
     :text (hsl->hex [bh (clamp (/ bs 4) 0 100)
                       (if dark? 91 13)])
     :text-muted (hsl->hex [bh (clamp (/ bs 3) 0 100)
                             (if dark? 55 40)])
     :accent (hsl->hex accent)
     :accent-hover (hsl->hex [ah as (clamp (if dark? (+ al 10) (- al 10)) 0 100)])
     :green (hsl->hex [140 (clamp (if dark? 60 55) 0 100)
                        (clamp (if dark? 45 30) 0 100)])
     :red (hsl->hex [0 (clamp (if dark? 80 70) 0 100)
                      (clamp (if dark? 55 40) 0 100)])}))

;;; ---------- Preset themes ----------

(def theme-presets
  [{:name "Midnight"  :bg [215 50 7]   :accent [212 100 67]}
   {:name "Daylight"  :bg [0 0 100]    :accent [212 100 44]}
   {:name "Amber"     :bg [30 40 10]   :accent [30 100 50]}
   {:name "Solarized" :bg [192 100 11] :accent [175 59 40]}
   {:name "Forest"    :bg [150 30 10]  :accent [140 60 55]}
   {:name "Dracula"   :bg [231 15 18]  :accent [265 89 78]}])

(def default-dark-preset (first theme-presets))
(def default-light-preset (second theme-presets))

;;; ---------- Theme load/save ----------

(defn load-theme []
  (or (try
        (when-let [raw (.getItem js/localStorage "calc-theme")]
          (let [parsed (js->clj (js/JSON.parse raw) :keywordize-keys true)]
            (when (and (:bg parsed) (:accent parsed))
              {:bg (vec (:bg parsed))
               :accent (vec (:accent parsed))
               :preset (:preset parsed)})))
        (catch :default _ nil))
      (if (and js/window.matchMedia
               (.-matches (.matchMedia js/window "(prefers-color-scheme: light)")))
        {:bg (:bg default-light-preset) :accent (:accent default-light-preset) :preset "Daylight"}
        {:bg (:bg default-dark-preset) :accent (:accent default-dark-preset) :preset "Midnight"})))

(defn load-fmt-opts []
  (try
    (when-let [raw (.getItem js/localStorage "calc-fmt-opts")]
      (js->clj (js/JSON.parse raw) :keywordize-keys true))
    (catch :default _ nil)))

(defn save-fmt-opts! [opts]
  (try
    (if opts
      (.setItem js/localStorage "calc-fmt-opts"
                (js/JSON.stringify (clj->js opts)))
      (.removeItem js/localStorage "calc-fmt-opts"))
    (catch :default _ nil)))

(defn load-default-fmt-opts []
  (try
    (when-let [raw (.getItem js/localStorage "calc-default-fmt-opts")]
      (js->clj (js/JSON.parse raw) :keywordize-keys true))
    (catch :default _ nil)))

(defn save-default-fmt-opts! [opts]
  (try
    (if (seq opts)
      (.setItem js/localStorage "calc-default-fmt-opts"
                (js/JSON.stringify (clj->js opts)))
      (.removeItem js/localStorage "calc-default-fmt-opts"))
    (catch :default _ nil)))

(defn load-precision []
  (try
    (when-let [raw (.getItem js/localStorage "calc-precision")]
      (let [n (js/parseInt raw 10)]
        (when-not (js/isNaN n) n)))
    (catch :default _ nil)))

(defn save-precision! [v]
  (try
    (if v
      (.setItem js/localStorage "calc-precision" (str v))
      (.removeItem js/localStorage "calc-precision"))
    (catch :default _ nil)))

(defn apply-precision! [n]
  (m/set-precision! n))

(defn benchmark-precision
  "Find the largest precision where 100 iterations of (10^(prec-1))+1-(10^(prec-1))
   complete in under 200ms. Returns the best precision value."
  []
  (let [candidates [200 500 1000 2000 5000 10000 20000 50000
                    100000 200000 500000 1000000]]
    (loop [remaining candidates
           best m/default-precision]
      (if (empty? remaining)
        best
        (let [prec (first remaining)
              _ (m/set-precision! prec)
              start (.now js/Date)
              _ (dotimes [_ 100]
                  (let [big (m/dpow 10 (dec prec))]
                    (m/d- (m/d+ big 1) big)))
              elapsed (- (.now js/Date) start)]
          (if (< elapsed 200)
            (recur (rest remaining) prec)
            (do (m/set-precision! best) best)))))))

(defn load-hide-examples []
  (try
    (= "true" (.getItem js/localStorage "calc-hide-examples"))
    (catch :default _ false)))

(defn save-hide-examples! [v]
  (try
    (if v
      (.setItem js/localStorage "calc-hide-examples" "true")
      (.removeItem js/localStorage "calc-hide-examples"))
    (catch :default _ nil)))

(defn load-completions-enabled []
  (try
    (not= "false" (.getItem js/localStorage "calc-completions"))
    (catch :default _ true)))

(defn save-completions-enabled! [v]
  (try
    (if v
      (.removeItem js/localStorage "calc-completions")
      (.setItem js/localStorage "calc-completions" "false"))
    (catch :default _ nil)))

(defn mobile? []
  (and js/window.matchMedia
       (.-matches (.matchMedia js/window "(max-width: 480px)"))))

(defn default-zoom []
  (if (mobile?) 1.2 2.0))

(defn load-zoom []
  (try
    (when-let [raw (.getItem js/localStorage "calc-zoom")]
      (let [n (js/parseFloat raw)]
        (when-not (js/isNaN n) n)))
    (catch :default _ nil)))

(defn save-zoom! [v]
  (try
    (if v
      (.setItem js/localStorage "calc-zoom" (str v))
      (.removeItem js/localStorage "calc-zoom"))
    (catch :default _ nil)))

(defn apply-zoom! [v]
  (set! (.. js/document -documentElement -style -zoom) (str v)))

(def initial-precision (or (load-precision) m/default-precision))
(apply-precision! initial-precision)

(defonce state (r/atom {:input ""
                        :result nil
                        :error nil
                        :history (load-history)
                        :fmt-opts (load-fmt-opts)
                        :default-fmt-opts (load-default-fmt-opts)
                        :hist-index -1
                        :saved-input ""
                        :menu-open false
                        :theme (load-theme)
                        :hide-examples (load-hide-examples)
                        :zoom (or (load-zoom) (default-zoom))
                        :precision initial-precision
                        :page (case (.. js/window -location -hash)
                                "#/help" :help
                                "#/settings" :settings
                                :calc)
                        :copied-idx nil
                        :comp-index -1
                        :show-completions false
                        :completions-enabled (load-completions-enabled)}))

(defn- page->hash [page]
  (case page :help "#/help" :settings "#/settings" ""))

(defn navigate! [page & kvs]
  (apply swap! state assoc :page page kvs)
  (let [h (page->hash page)]
    (when (not= (.. js/window -location -hash) h)
      (if (= h "")
        (.pushState js/history nil "" (.. js/window -location -pathname))
        (.pushState js/history nil "" h))))
  (js/setTimeout #(when-let [el (.querySelector js/document "main")] (set! (.-scrollTop el) 0)) 0))

(.addEventListener js/window "popstate"
  (fn [_]
    (let [page (case (.. js/window -location -hash)
                 "#/help" :help
                 "#/settings" :settings
                 :calc)]
      (swap! state assoc :page page))))

(defn effective-fmt-opts
  "Merge default settings with session overrides. Session wins."
  []
  (let [{:keys [default-fmt-opts fmt-opts]} @state]
    (merge default-fmt-opts fmt-opts)))

(defonce log-ref (atom nil))
(defonce suppress-menu (atom false))
(defonce press-timer (atom nil))
(defonce blur-timer (atom nil))
(def long-press-ms 400)

(defn scroll-log-to-top []
  (when-let [el @log-ref]
    (set! (.-scrollTop el) 0)))

(defn apply-theme! [theme-map]
  (let [colors (derive-theme (:bg theme-map) (:accent theme-map))
        style (.-style (.-documentElement js/document))]
    (doseq [[k v] colors]
      (.setProperty style (str "--" (name k)) v))
    (when-let [meta-el (.querySelector js/document "meta[name='theme-color']")]
      (.setAttribute meta-el "content" (:bg colors)))))

(defn save-theme! [theme-map]
  (try
    (.setItem js/localStorage "calc-theme"
              (js/JSON.stringify (clj->js theme-map)))
    (catch :default _ nil)))

(defn set-theme! [theme-map]
  (swap! state assoc :theme theme-map)
  (save-theme! theme-map)
  (apply-theme! theme-map))

(def examples
  ["100GB / 900Mbps"
   "(10^100)+1-(10^100)"
   "2 cups in tablespoons"
   "3 feet in inches"
   "37 W / 12 v"
   "5 feet 11 inches to cm"
   "300 miles / 65 mph in hours and minutes"
   "60 mph * 2 hours"
   "3.53 hours in minutes and seconds"
   "180cm in feet and inches"
   "10 is what percent of 250?"
   "100 fahrenheit to celsius"
   "60 mph in ft/s"
   "1 GB in MB"
   "3.5 kg to pounds"
   "2 cubic yards to gallons"
   "5 liters in gallons"
   "7 inches in feet as a fraction"
   "15% of 50"
   "sqrt(144)"
   "square root of 2"
   "cube root of 27"
   "4th root of 625"
   "2 * sqrt(25)"
   "2 + 2"
   "3 * (4 + 5)"
   "1e9 BTU in kWh"
   "tip $50"
   "tip 85.50 20"
   "tax 29.99 8.25"
   "download 10GB 1Gbps"
   "roll 2d6"])

(def unit-groups units/unit-groups)

(defn example-chip [text]
  [:button.example
   {:on-click (fn []
                (swap! state assoc :input text)
                (when-let [el (.querySelector js/document ".input-wrapper input")]
                  (.focus el)))}
   text])

(def help-example-groups
  [["Unit Conversion"
    [["12 feet in yards" "4 yd"]
     ["5 miles to km" "8.04672 km"]
     ["100 fahrenheit to celsius" "37.7778 \u00b0C"]
     ["how many inches are in 3 feet?" "36 in"]
     ["3.5 kg to pounds" "7.71618 lb"]
     ["10e9 bytes in GB" "10 GB"]]]
   ["Mixed Quantities"
    [["5 feet 11 inches to cm" "180.34 cm"]
     ["1 hour 30 minutes in seconds" "5400 s"]
     ["6 lb 4 oz in grams" "2834.9523 g"]]]
   ["Mixed Output"
    [["180cm in feet and inches" "5 ft 10.866... in"]
     ["90 minutes in hours and minutes" "1 hr 30 min"]
     ["10000 seconds in hours, minutes, and seconds" "2 hr 46 min 40 s"]
     ["200 lb in stone and pounds" "14 st 4 lb"]]]
   ["Area & Volume"
    [["10 square feet in square meters" "0.9290304 m\u00b2"]
     ["2 cubic yards to gallons" "403.948 gal"]
     ["100 sqft in sqm" "9.290304 m\u00b2"]]]
   ["Compound Units"
    [["60 mph in ft/s" "88 ft/s"]
     ["100 GB / 900 Mbps" "14.81 min"]]]
   ["Roots"
    [["sqrt(144)" "12"]
     ["square root of 2" "1.4142..."]
     ["cube root of 27" "3"]
     ["4th root of 625" "5"]
     ["fifth root of 32" "2"]
     ["root(3, 125)" "5"]
     ["2 * sqrt(25)" "10"]]]
   ["Percentages"
    [["15% of 50" "7.5"]
     ["10 is what percent of 100?" "10%"]
     ["what is 25 percent of 200" "50"]]]
   ["Math"
    [["2 + 2" "4"]
     ["3 * (4 + 5)" "27"]
     ["2^10" "1024"]
     ["(10^100)+1-(10^100)" "1"]
     ["sqrt(9) + sqrt(16)" "7"]]]
   ["Tip & Tax"
    [["tip $50" "Tip table with 15%, 20%, round options"]
     ["tip 85 18" "18% exact + round total option"]
     ["tip $100 15%" "15% exact + round total option"]
     ["tax 29.99 8.25" "Price: $29.99, Tax: $2.48 (8.25%), Total: $32.47"]
     ["tax $50 10%" "Price: $50, Tax: $5 (10%), Total: $55"]]]
   ["Download Time"
    [["download 10GB 1Gbps" "10 GB at 1 Gbps \u2192 1 min 20 s"]
     ["download 1g 1g" "1 GB at 1 Gbps \u2192 8 s"]
     ["upload 100MB 10Mbps" "100 MB at 10 Mbps \u2192 1 min 20 s"]]]
   ["Formatting"
    [["7 inches in feet as a fraction" "7/12 ft"]
     ["square root of 2 rounded to 4 decimals" "1.4142"]
     ["5 miles in km with 3 sig figs" "8.05 km"]]]])

(defn- run-example [input]
  (swap! state assoc :input input)
  (let [ev (evaluate input (effective-fmt-opts))]
    (swap! state assoc
           :result (:result ev)
           :error (:error ev)
           :input "")
    (swap! state update :history
           (fn [h]
             (into [{:input input
                     :from (:from ev)
                     :target (:target ev)
                     :result (:result ev)
                     :error (:error ev)}]
                   h)))
    (navigate! :calc)
    (save-history! (:history @state))
    (js/setTimeout scroll-log-to-top 0)))

(defn help-page []
  [:div.help-page
   [:div.help-header
    [:button.back-btn
     {:on-click #(navigate! :calc)}
     "\u2190 Back"]
    [:h2 "Help"]]
   (let [snapshot-meta (.querySelector js/document "meta[name='calc-snapshot']")
         snapshot-sha (when snapshot-meta (.getAttribute snapshot-meta "content"))
         git-sha (some-> (.querySelector js/document "meta[name='calc-git-sha']")
                         (.getAttribute "content"))
         dev? (= git-sha "dev")
         mode (cond snapshot-sha :snapshot dev? :dev :else :pwa)]
     [:div
      [:p.help-intro
       [:strong "calc"] " is a unit conversion calculator that understands natural English. "
       "It supports dimensional analysis across length, weight, volume, temperature, speed, time, data, and more. "
       (case mode
         :snapshot
         [:<>
          "This is a static snapshot build (" snapshot-sha "). Visit "
          [:a {:href "https://calc.rymcg.tech" :target "_blank" :rel "noopener"} "calc.rymcg.tech"]
          " to download the latest release."]

         :dev
         "This is a development build."

         :pwa
         [:<>
          "This page is a static HTML/JS PWA (Progressive Web App) - all calculations are performed client-side in your browser. "
          "You can install this page from your browser menu to your desktop / home screen and run it offline as an app. "
          "You can also download a single self-contained file you can run from anywhere."])]
      (when (= mode :pwa)
        [:div {:style {:text-align "center" :margin-bottom "1.5rem"}}
         [:a.download-btn {:href "/calc.html" :download "calc.html"} "Download"]])])
   (for [[group-name entries] help-example-groups]
     ^{:key group-name}
     [:div.unit-group
      [:h3 group-name]
      [:div.unit-table {:style {:grid-template-columns "1fr"}}
       (for [[input output] entries]
         ^{:key input}
         [:div.unit-row {:style {:cursor "pointer"}
                         :on-click #(run-example input)}
          [:code {:style {:flex "1"}} input]
          [:span.unit-label {:style {:text-align "right"}} (str "\u2192 " output)]])]])
   [:div.unit-group
    [:h3 "Commands"]
    [:div.unit-table
     [:div.unit-row [:span.unit-sym "/help"] [:span.unit-label "Show this help page"]]
     [:div.unit-row [:span.unit-sym "/p N"] [:span.unit-label "Set precision to N decimals (session)"]]
     [:div.unit-row [:span.unit-sym "/p"] [:span.unit-label "Clear session precision"]]
     [:div.unit-row [:span.unit-sym "/s N"] [:span.unit-label "Set sig-figs to N (session)"]]
     [:div.unit-row [:span.unit-sym "/s"] [:span.unit-label "Clear session sig-figs"]]
     [:div.unit-row [:span.unit-sym "clear"] [:span.unit-label "Clear all history"]]]]
   [:h2 {:style {:margin-top "1.5rem" :margin-bottom "0.5rem" :font-size "1.1rem" :color "var(--accent)"}} "Available Units"]
   (for [{group-name :name :keys [description units]} unit-groups]
     ^{:key group-name}
     [:div.unit-group
      [:h3 group-name]
      [:p.group-desc description]
      [:div.unit-table
       (for [[sym label] units]
         ^{:key sym}
         [:div.unit-row
          [:span.unit-sym (name sym)]
          [:span.unit-label label]])]])])

(defn hsl-slider [label h-key s-key l-key theme on-change-fn]
  (let [[h s l] [(get theme h-key) (get theme s-key) (get theme l-key)]
        swatch-color (hsl->hex [(get theme h-key) (get theme s-key) (get theme l-key)])]
    [:<>
     [:div.setting-row
      [:label.setting-label label]
      [:div.color-swatch {:style {:background swatch-color}}]]
     [:div.setting-row
      [:label.setting-label {:style {:min-width "1.5em"}} "H"]
      [:input {:type "range" :min 0 :max 360 :step 1 :value h
               :style {:width "100%"}
               :on-change (fn [e] (on-change-fn h-key (js/parseInt (.. e -target -value) 10)))}]]
     [:div.setting-row
      [:label.setting-label {:style {:min-width "1.5em"}} "S"]
      [:input {:type "range" :min 0 :max 100 :step 1 :value s
               :style {:width "100%"}
               :on-change (fn [e] (on-change-fn s-key (js/parseInt (.. e -target -value) 10)))}]]
     [:div.setting-row
      [:label.setting-label {:style {:min-width "1.5em"}} "L"]
      [:input {:type "range" :min 0 :max 100 :step 1 :value l
               :style {:width "100%"}
               :on-change (fn [e] (on-change-fn l-key (js/parseInt (.. e -target -value) 10)))}]]]))

(defn theme-sliders []
  (let [theme (:theme @state)
        [bh bs bl] (:bg theme)
        [ah as al] (:accent theme)
        ;; Use a flat map for slider state so each slider has its own key
        slider-state {:bh bh :bs bs :bl bl :ah ah :as as :al al}
        update-and-apply (fn [k v]
                           (let [cur (:theme @state)
                                 [obh obs obl] (:bg cur)
                                 [oah oas oal] (:accent cur)
                                 new-theme (case k
                                             :bh (assoc cur :bg [v obs obl] :preset nil)
                                             :bs (assoc cur :bg [obh v obl] :preset nil)
                                             :bl (assoc cur :bg [obh obs v] :preset nil)
                                             :ah (assoc cur :accent [v oas oal] :preset nil)
                                             :as (assoc cur :accent [oah v oal] :preset nil)
                                             :al (assoc cur :accent [oah oas v] :preset nil))]
                             (set-theme! new-theme)))]
    [:<>
     [hsl-slider "Background" :bh :bs :bl slider-state update-and-apply]
     [:div {:style {:height "0.5rem"}}]
     [hsl-slider "Accent" :ah :as :al slider-state update-and-apply]]))

(defn settings-calculator-tab []
  (let [defaults (or (:default-fmt-opts @state) {})
        has-round (contains? defaults :round)
        has-sigs (contains? defaults :sig-figs)
        round-val (or (:round defaults) 4)
        sigs-val (or (:sig-figs defaults) 6)]
    [:<>
     [:div.settings-section
      [:h3 "Auto Completion"]
      [:div.setting-row
       [:label.setting-label
        [:input {:type "checkbox"
                 :checked (:completions-enabled @state)
                 :on-change
                 (fn [_]
                   (let [v (not (:completions-enabled @state))]
                     (swap! state assoc :completions-enabled v :show-completions false)
                     (save-completions-enabled! v)))}]
        "Enable suggestions dropdown"]]]
     [:div.settings-section
      [:h3 "History"]
      [:div.setting-row
       [:label.setting-label
        [:input {:type "checkbox"
                 :checked (:hide-examples @state)
                 :on-change
                 (fn [_]
                   (let [v (not (:hide-examples @state))]
                     (swap! state assoc :hide-examples v)
                     (save-hide-examples! v)))}]
        "Hide examples cloud"]]]
     [:div.settings-section
      [:h3 "Default Formatting"]
      [:p.group-desc
       "Set default precision for all calculations. "
       "Use /p and /s commands to override per session."]
      [:div.setting-row
       [:label.setting-label
        [:input {:type "checkbox"
                 :checked has-round
                 :on-change
                 (fn [_]
                   (let [new-opts (if has-round
                                   (dissoc defaults :round)
                                   (-> defaults
                                       (dissoc :sig-figs)
                                       (assoc :round round-val)))]
                     (swap! state assoc :default-fmt-opts new-opts)
                     (save-default-fmt-opts! new-opts)))}]
        "Decimal places"]
       (when has-round
         [:input.setting-input
          {:type "number" :min 0 :max 20 :value round-val
           :on-change
           (fn [e]
             (let [n (js/parseInt (.. e -target -value) 10)]
               (when-not (js/isNaN n)
                 (let [new-opts (assoc defaults :round n)]
                   (swap! state assoc :default-fmt-opts new-opts)
                   (save-default-fmt-opts! new-opts)))))}])]
      [:div.setting-row
       [:label.setting-label
        [:input {:type "checkbox"
                 :checked has-sigs
                 :on-change
                 (fn [_]
                   (let [new-opts (if has-sigs
                                   (dissoc defaults :sig-figs)
                                   (-> defaults
                                       (dissoc :round)
                                       (assoc :sig-figs sigs-val)))]
                     (swap! state assoc :default-fmt-opts new-opts)
                     (save-default-fmt-opts! new-opts)))}]
        "Significant figures"]
       (when has-sigs
         [:input.setting-input
          {:type "number" :min 1 :max 20 :value sigs-val
           :on-change
           (fn [e]
             (let [n (js/parseInt (.. e -target -value) 10)]
               (when-not (js/isNaN n)
                 (let [new-opts (assoc defaults :sig-figs n)]
                   (swap! state assoc :default-fmt-opts new-opts)
                   (save-default-fmt-opts! new-opts)))))}])]]
     (let [prec (:precision @state)
           benchmarking? (:benchmarking @state)]
       [:div.settings-section
        [:h3 "Arithmetic Precision"]
        [:p.group-desc
         "Maximum significant digits for decimal.js arithmetic. "
         "Higher values support larger exponents (e.g. 10^N) but use more memory. "
         "Default: " m/default-precision "."]
        [:div.setting-row
         [:label.setting-label (str "Digits: " prec)]
         [:input.setting-input
          {:type "number" :min 34 :value prec
           :style {:width "6em"}
           :on-change
           (fn [e]
             (let [n (js/parseInt (.. e -target -value) 10)]
               (when (and (not (js/isNaN n)) (>= n 34))
                 (swap! state assoc :precision n)
                 (apply-precision! n)
                 (save-precision! n))))}]]
        [:div.setting-row
         [:button.back-btn
          {:disabled benchmarking?
           :on-click (fn []
                       (swap! state assoc :benchmarking true)
                       (js/setTimeout
                        (fn []
                          (let [result (benchmark-precision)]
                            (apply-precision! result)
                            (save-precision! result)
                            (swap! state assoc
                                   :precision result
                                   :benchmarking false)))
                        50))}
          (if benchmarking? "Running..." "Auto")]
         [:button.back-btn
          {:on-click (fn []
                       (apply-precision! m/default-precision)
                       (save-precision! nil)
                       (swap! state assoc :precision m/default-precision))}
          "Reset"]]])]))

(defn settings-appearance-tab []
  (let [theme (:theme @state)]
    [:<>
     [:div.settings-section
      [:h3 "Theme"]
      [:div.preset-row
       (for [{:keys [name bg accent]} theme-presets]
         ^{:key name}
         [:button.preset-chip
          {:class (when (= name (:preset theme)) "active")
           :style {:background (hsl->hex bg)
                   :color (hsl->hex [(first accent) (second accent) (nth accent 2)])}
           :on-click (fn [] (set-theme! {:bg bg :accent accent :preset name}))}
          name])]
      [theme-sliders]
      [:div.setting-row
       [:button.back-btn
        {:on-click (fn []
                     (let [preset (or (some #(when (= (:name %) (:preset theme)) %) theme-presets)
                                      default-dark-preset)]
                       (set-theme! {:bg (:bg preset) :accent (:accent preset) :preset (:name preset)})))}
        "Reset"]]]
     [:div.settings-section
      [:h3 "Zoom"]
      (let [pending (or (:zoom-pending @state) (:zoom @state))]
        [:<>
         [:div.setting-row
          [:label.setting-label (str "Zoom: " (.toFixed (js/Number pending) 1))]
          [:input {:type "range"
                   :min 0.8 :max 2.0 :step 0.1
                   :value pending
                   :style {:width "100%"}
                   :on-change (fn [e]
                                (let [v (js/parseFloat (.. e -target -value))]
                                  (swap! state assoc :zoom-pending v)))}]]
         [:div.setting-row
          [:button.back-btn
           {:on-click (fn []
                        (let [v (or (:zoom-pending @state) (:zoom @state))]
                          (swap! state assoc :zoom v :zoom-pending nil)
                          (save-zoom! v)
                          (apply-zoom! v)))}
           "Apply"]
          [:button.back-btn
           {:on-click (fn []
                        (let [d (default-zoom)]
                          (swap! state assoc :zoom d :zoom-pending nil)
                          (save-zoom! nil)
                          (apply-zoom! d)))}
           "Reset"]]])]]))

(defn settings-page []
  (let [tab (or (:settings-tab @state) :calculator)]
    [:div.settings-page
     [:div.help-header
      [:button.back-btn
       {:on-click #(navigate! :calc)}
       "\u2190 Back"]
      [:h2 "Settings"]]
     [:div.settings-tabs
      [:button.settings-tab
       {:class (when (= tab :calculator) "active")
        :on-click #(swap! state assoc :settings-tab :calculator)}
       "Calculator"]
      [:button.settings-tab
       {:class (when (= tab :appearance) "active")
        :on-click #(swap! state assoc :settings-tab :appearance)}
       "Appearance"]]
     (case tab
       :appearance [settings-appearance-tab]
       [settings-calculator-tab])]))

(def clear-commands #{"clear" "/clear" "reset" "/reset"})

(defn clear-history! []
  (swap! state assoc :input "" :result nil :error nil :history [] :fmt-opts nil)
  (save-history! []))

(defn delete-history-entry! [idx]
  (swap! state update :history (fn [h] (into [] (concat (subvec h 0 idx) (subvec h (inc idx))))))
  (save-history! (:history @state)))

(defn parse-slash-command
  "Parse a slash command. Returns {:cmd name :arg value} or nil."
  [input]
  (when (str/starts-with? input "/")
    (let [parts (str/split (subs input 1) #"\s+" 2)]
      {:cmd (first parts) :arg (second parts)})))

(defn- handle-slash-command
  "Handle a slash command. Returns a history entry map to display, or nil for clear."
  [{:keys [cmd arg]}]
  (case cmd
    "help"
    (do (navigate! :help) {:input "/help" :result "Showing help page"})

    "p"
    (if (str/blank? arg)
      (do (swap! state update :fmt-opts dissoc :round)
          {:input "/p" :result "Precision cleared (session)"})
      (let [n (js/parseInt arg 10)]
        (if (js/isNaN n)
          {:input (str "/p " arg) :error "/p requires a number"}
          (do (swap! state assoc :fmt-opts (-> (or (:fmt-opts @state) {})
                                               (dissoc :sig-figs)
                                               (assoc :round n)))
              {:input (str "/p " n) :result (str "Precision set to " n " decimal places (session)")}))))

    "s"
    (if (str/blank? arg)
      (do (swap! state update :fmt-opts dissoc :sig-figs)
          {:input "/s" :result "Sig-figs cleared (session)"})
      (let [n (js/parseInt arg 10)]
        (if (js/isNaN n)
          {:input (str "/s " arg) :error "/s requires a number"}
          (do (swap! state assoc :fmt-opts (-> (or (:fmt-opts @state) {})
                                               (dissoc :round)
                                               (assoc :sig-figs n)))
              {:input (str "/s " n) :result (str "Sig-figs set to " n " (session)")}))))

    ;; unknown
    {:input (str "/" cmd) :error (str "Unknown command: /" cmd)}))

(defn evaluate! []
  (let [input (parser/clean-phrase (:input @state))]
    (when-not (str/blank? input)
      (cond
        (clear-commands (str/lower-case input))
        (clear-history!)

        (and (str/starts-with? input "/")
             (not (clear-commands (str/lower-case input))))
        (let [parsed (parse-slash-command input)
              entry (handle-slash-command parsed)]
          (swap! state assoc :input "" :result (:result entry) :error (:error entry))
          (swap! state update :history (fn [h] (into [entry] h)))
          (save-history! (:history @state))
          (js/setTimeout scroll-log-to-top 0))

        :else
        (let [ev (evaluate input (effective-fmt-opts))]
          (swap! state assoc
                 :result (:result ev)
                 :error (:error ev)
                 :input "")
          (swap! state update :history
                 (fn [h]
                   (into [{:input input
                           :from (:from ev)
                           :target (:target ev)
                           :result (:result ev)
                           :error (:error ev)}]
                         h)))
          (navigate! :calc)
          (save-history! (:history @state))
          (js/setTimeout scroll-log-to-top 0))))))

(def ^:private max-web-completions 20)

(defn current-completions
  "Derive completions from the current input. Always fresh, never stale."
  [input]
  (->> (completions/complete input)
       (take max-web-completions)
       (sort-by (juxt :group :text))
       vec))

(defn on-input-change [e]
  (when-let [t @blur-timer] (js/clearTimeout t) (reset! blur-timer nil))
  (let [val (.. e -target -value)]
    (swap! state assoc
           :input val
           :hist-index -1
           :comp-index -1
           :show-completions (:completions-enabled @state))))

(defn accept-completion
  "Replace the current prefix in :input with the completion text, add a trailing space."
  [completion-text]
  (let [input (:input @state)
        buf (or input "")
        trimmed (str/trim buf)
        at-space? (and (seq buf) (= \space (last buf)))
        parts (if (str/blank? trimmed) [] (str/split trimmed #"\s+"))
        prefix (if at-space? "" (or (last parts) ""))
        base (if at-space?
               buf
               (let [idx (str/last-index-of buf prefix)]
                 (if idx (subs buf 0 idx) buf)))
        new-input (str base completion-text " ")]
    (swap! state assoc
           :input new-input
           :comp-index -1
           :show-completions true)
    (when-let [el (.querySelector js/document ".input-wrapper input")]
      (js/setTimeout
       (fn []
         (.focus el)
         (let [len (count new-input)]
           (.setSelectionRange el len len)))
       0))))

(defn on-keydown [e]
  (let [key (.-key e)
        {:keys [history hist-index saved-input input comp-index show-completions]} @state
        comps (when show-completions (current-completions input))
        all-hints? (and (seq comps) (every? :hint comps))
        has-completions? (and (seq comps) (not all-hints?))]
    (case key
      "Tab"
      (when has-completions?
        (.preventDefault e)
        (if (= comp-index -1)
          (swap! state assoc :comp-index 0)
          (let [dir (if (.-shiftKey e) -1 1)
                new-idx (mod (+ comp-index dir) (count comps))]
            (swap! state assoc :comp-index new-idx))))

      "Enter"
      (if (and has-completions? (>= comp-index 0))
        (do (.preventDefault e)
            (accept-completion (:text (nth comps comp-index))))
        (do (evaluate!)
            (swap! state assoc :hist-index -1 :saved-input ""
                   :comp-index -1 :show-completions false)))

      "ArrowDown"
      (if has-completions?
        (do (.preventDefault e)
            (swap! state assoc :comp-index
                   (min (inc (max comp-index -1)) (dec (count comps)))))
        (when (>= hist-index 0)
          (.preventDefault e)
          (let [new-idx (dec hist-index)]
            (if (neg? new-idx)
              (swap! state assoc :hist-index -1 :input saved-input)
              (swap! state assoc
                     :hist-index new-idx
                     :input (:input (nth history new-idx)))))))

      "ArrowUp"
      (if has-completions?
        (do (.preventDefault e)
            (swap! state assoc :comp-index (max (dec comp-index) 0)))
        (let [max-idx (dec (count history))
              new-idx (min (inc hist-index) max-idx)]
          (when (and (seq history) (not= new-idx hist-index))
            (.preventDefault e)
            (when (= hist-index -1)
              (swap! state assoc :saved-input input))
            (swap! state assoc
                   :hist-index new-idx
                   :input (:input (nth history new-idx))))))

      "Escape"
      (do (.preventDefault e)
          (if has-completions?
            (swap! state assoc :comp-index -1 :show-completions false)
            (swap! state assoc :input "" :hist-index -1)))

      nil)))

(defn- completion-item-view [abs-idx item comp-index]
  (if (:hint item)
    [:div.completion-item.completion-hint-item
     [:span.completion-text (:text item)]
     (when (:desc item)
       [:span.completion-desc (:desc item)])]
    [:div.completion-item
     {:class (when (= abs-idx comp-index) "highlighted")
      :ref (fn [el]
             (when (and el (= abs-idx comp-index))
               (.scrollIntoView el #js {:block "nearest"})))
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (accept-completion (:text item)))}
     [:span.completion-text (:text item)]
     (when (:desc item)
       [:span.completion-desc (:desc item)])]))

(defn- flatten-with-headers [indexed-comps]
  (loop [items indexed-comps, prev-group nil, result []]
    (if (empty? items)
      result
      (let [[idx entry] (first items)
            group (:group entry)
            result (if (= group prev-group)
                     result
                     (conj result [:group group]))]
        (recur (rest items) group (conj result [:item idx entry]))))))

(defn completion-dropdown [preview]
  (let [{:keys [input comp-index show-completions]} @state
        comps (when show-completions (current-completions input))
        dim-hint (when show-completions (completions/target-dim-hint input))]
    (when (seq comps)
      (let [elements (flatten-with-headers (map-indexed vector comps))
            children (for [element elements]
                       (case (first element)
                         :group ^{:key (str "g-" (second element))}
                                [:div.completion-group (second element)]
                         :item (let [[_ idx entry] element]
                                 ^{:key (str "i-" idx)}
                                 (completion-item-view idx entry comp-index))))
            preview-el (when (and preview (not (:error preview)))
                         [:div.completion-preview
                          {:key "preview"}
                          (cond
                            (and (:result preview) (str/includes? (str (:result preview)) "\n"))
                            [multiline-result (:result preview) "preview-result"]
                            (:target preview)
                            [:span.preview-result (str "= " (:result preview) " " (:target preview))]
                            :else
                            [:span.preview-result (str "= " (:result preview))])])]
        [:div.completion-dropdown
         (into [:div.completion-items]
               (if dim-hint
                 (cons ^{:key "hint"} [:div.completion-hint (str "Expected: " dim-hint)]
                       children)
                 children))
         preview-el]))))

(defn app []
  (let [{:keys [input history menu-open]} @state
        eff-fmt (effective-fmt-opts)
        typing? (not (str/blank? input))
        preview (when (and typing?
                           (not (str/starts-with? (str/trim input) "/")))
                  (or (fmt/roll-preview (str/trim input))
                      (evaluate input eff-fmt)))]
    [:<>
     [:header
      [:h1 {:on-click (fn [_]
                        (navigate! :calc :input "" :hist-index -1)
                        (js/window.scrollTo 0 0)
                        (when-let [el (.querySelector js/document "header input[type='text']")]
                          (.focus el)))}
       "λ"]
      [:div.input-wrapper
       [:input (cond-> {:type "text"
                        :value input
                        :auto-focus true
                        :auto-complete "off"
                        :on-change on-input-change
                        :on-key-down on-keydown
                        :on-blur (fn [_]
                                   (when-let [t @blur-timer] (js/clearTimeout t))
                                   (reset! blur-timer
                                           (js/setTimeout
                                            (fn []
                                              (reset! blur-timer nil)
                                              (swap! state assoc :comp-index -1 :show-completions false))
                                            150)))}
                 (empty? history) (assoc :placeholder "e.g. 100GB / 900Mbps"))]
       [completion-dropdown preview]
       (let [clear-fn (fn [e]
                        (.preventDefault e)
                        (.stopPropagation e)
                        (reset! suppress-menu true)
                        (js/setTimeout #(reset! suppress-menu false) 300)
                        (swap! state assoc :input "" :hist-index -1)
                        (let [input-el (some-> (.-target e) .-parentElement (.querySelector "input"))]
                          (js/setTimeout #(when input-el (.blur input-el)) 100)))]
         [:button {:class (str "clear-input" (when (str/blank? input) " empty"))
                   :on-mouse-down clear-fn
                   :on-touch-start clear-fn
                   :on-click (fn [e] (.stopPropagation e))} "\u00d7"])]
      [:button.menu-btn {:on-click #(when-not @suppress-menu (swap! state update :menu-open not))}
       [:span.hamburger]
       [:span.hamburger]
       [:span.hamburger]]]

     (when menu-open
       [:<>
        [:div.menu-overlay {:on-click #(swap! state assoc :menu-open false)}]
        [:nav.menu
         [:button.menu-item
          {:on-click (fn []
                      (navigate! :calc :menu-open false))}
          "Home"]
         [:button.menu-item
          {:on-click (fn []
                      (navigate! :help :menu-open false))}
          "Help"]
         [:button.menu-item
          {:on-click (fn []
                      (clear-history!)
                      (swap! state assoc :menu-open false))}
          "Clear History"]
         [:button.menu-item
          {:on-click (fn []
                      (navigate! :settings :menu-open false))}
          "Settings"]
         [:a.menu-item
          {:href "https://github.com/EnigmaCurry/calc"
           :target "_blank"
           :rel "noopener"
           :on-click #(swap! state assoc :menu-open false)}
          "Source Code"]
         (when-let [sha (some-> (.querySelector js/document "meta[name='calc-git-sha']")
                                (.getAttribute "content"))]
           (when-not (or (= sha "__GIT_SHA__") (= sha "dev"))
             (let [url (str "https://github.com/EnigmaCurry/calc/commit/" sha)]
               [:a.sha-link {:href url :target "_blank" :rel "noopener"
                              :on-click #(swap! state assoc :menu-open false)}
                (str "#" sha)])))]])

     [:main {:ref #(reset! log-ref %)}
      (let [has-completions? (and (:show-completions @state)
                                  (seq (current-completions input)))]
        (when (and preview (not has-completions?))
          (let [incomplete? (and (seq input)
                                (or (= \space (last input))
                                    (re-find #"\d%$" input)))]
            [:div.preview-bar
             [:span.preview-spacer {:aria-hidden "true"} "calc"]
             [:span.preview-answer
              (cond
                (:error preview)
                (if incomplete?
                  [:span.preview-warning "keep typing\u2026"]
                  [:span.preview-error (:error preview)])

                (and (:result preview) (str/includes? (str (:result preview)) "\n"))
                [multiline-result (:result preview) "preview-result"]

                (:target preview)
                [:span.preview-result (str "= " (:result preview) " " (:target preview))]

                :else
                [:span.preview-result (str "= " (:result preview))])]
             [:button.convert {:on-click evaluate!} "="]])))
      (case (:page @state)
        :help [help-page]
        :settings [settings-page]
        [:<>
         (when (empty? history)
           [:div.empty-state
            [:div.empty-lambda "\u03bb"]
            [:div.empty-bubble "Type a conversion or calculation above"]
            [:div.empty-bubble "Results will appear here"]
            [:div.empty-bubble "Try \"5 feet in cm\" or \"2+2\""]
            [:div.empty-bubble "100% private and all calculations run locally"]
            [:div.empty-bubble "History is saved to your browser\u2019s local storage"]
            [:button.empty-bubble.empty-link {:on-click #(navigate! :help)} "View Help"]])
         (when (seq history)
           [:div.log
            (for [[idx {:keys [input from target result error]}] (map-indexed vector history)]
              (let [result-text (str (or from input) " = "
                                     (cond
                                       error error
                                       target (str result " " target)
                                       :else (str result)))
                    copied? (= idx (:copied-idx @state))
                    on-press-start (fn [e]
                                     (when-not (.. e -target -classList (contains "log-delete"))
                                       (when-let [old @press-timer] (js/clearTimeout old))
                                       (reset! press-timer
                                               (js/setTimeout
                                                (fn []
                                                  (reset! press-timer :fired)
                                                  (.writeText js/navigator.clipboard result-text)
                                                  (swap! state assoc :copied-idx idx)
                                                  (js/setTimeout #(swap! state assoc :copied-idx nil) 1200))
                                                long-press-ms))))
                    on-press-cancel (fn [_e]
                                      (when-let [timer @press-timer]
                                        (when (not= timer :fired)
                                          (js/clearTimeout timer))
                                        (reset! press-timer nil)))
                    on-press-end (fn [_e]
                                   (let [timer @press-timer]
                                     (when (and timer (not= timer :fired))
                                       (js/clearTimeout timer)
                                       (reset! press-timer nil)
                                       ;; Short click: put input in input box
                                       (when input
                                         (swap! state assoc :input input)
                                         (when-let [el (.querySelector js/document ".input-wrapper input")]
                                           (.focus el)
                                           (js/setTimeout
                                            (fn []
                                              (let [len (count input)]
                                                (.setSelectionRange el len len)))
                                            0))))))]
                ^{:key idx}
                [(if (and (zero? idx) (not typing?)) :div.log-entry.latest :div.log-entry)
                 {:on-mouse-down on-press-start
                  :on-mouse-up on-press-end
                  :on-touch-start on-press-start
                  :on-touch-end on-press-end
                  :on-touch-move on-press-cancel
                  :on-touch-cancel on-press-cancel
                  :on-click (fn [e] (.preventDefault e))}
                 [:span.log-input (or from input)]
                 (if copied?
                   [:span.log-copied "Copied!"]
                   (cond
                     error
                     [:span.log-error (str "\u2192 " error)]

                     (and result (str/includes? (str result) "\n"))
                     [multiline-result result "log-result"]

                     target
                     [:span.log-result (str "= " result " " target)]

                     :else
                     [:span.log-result (str "= " result)]))
                 [:button.log-delete
                  {:on-click (fn [e]
                               (.stopPropagation e)
                               (delete-history-entry! idx))}
                  "\u00d7"]]))])
         (when-not (:hide-examples @state)
           [:div.examples
            [:h3 "Try some examples"]
            [:div.chips
             (for [ex examples]
               ^{:key ex} [example-chip ex])]])])]]))

(defonce root (atom nil))

(defn ^:export init []
  (let [theme (load-theme)]
    (apply-theme! theme))
  (apply-zoom! (or (load-zoom) (default-zoom)))
  (let [el (js/document.getElementById "app")]
    (when-not @root
      (reset! root (rdom/create-root el)))
    (rdom/render @root [app])))
