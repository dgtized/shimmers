(ns shimmers.sketches.wave-function-collapse
  (:require
   [cljs.core.async :as async :include-macros true]
   [clojure.set :as set]
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def color-map
  {"A" "#4444dd"
   "B" "#cdcd00"
   "C" "#228b22"})

(defn cancel-active! [state]
  (when-let [cancel (:cancel @state)]
    (async/close! cancel)
    (swap! state assoc
           :message nil
           :cancel nil)))

(defn set-cell! [state loc value]
  (cancel-active! state)
  (let [{:keys [wfc-state]} @state
        {:keys [tiles grid rules]} wfc-state
        values (if (= (count (get grid loc)) 1)
                 tiles
                 #{value})

        [legal-tiles _] (wfc/legal-at-location grid rules loc)
        allowed-tiles  (set/intersection legal-tiles values)
        [changes grid'] (wfc/propagate grid rules loc allowed-tiles)]
    (swap! state assoc
           :highlight (conj changes loc)
           :wfc-state (assoc wfc-state :grid grid'))))

(def subdivisions {1 {:cols 1 :rows 1}
                   2 {:cols 1 :rows 2}
                   3 {:cols 3 :rows 1}
                   4 {:cols 2 :rows 2}
                   5 {:cols 2 :rows 3}
                   6 {:cols 3 :rows 2}
                   7 {:cols 3 :rows 3}
                   8 {:cols 3 :rows 3}
                   9 {:cols 3 :rows 3}
                   10 {:cols 4 :rows 3}
                   11 {:cols 4 :rows 3}
                   12 {:cols 4 :rows 3}})

(defn cell-tile [value piece]
  (if (string? value)
    (vary-meta piece assoc :fill (get color-map value))
    (->> (g/subdivide piece {:cols (count (first value)) :rows (count value)})
         (map (fn [pixel cell]
                (vary-meta cell assoc :fill (get color-map pixel)))
              (seq (apply str value))))))

(defn cell-subdivisions
  [cell loc values {:keys [tiles on-click]}]
  (let [options (count values)
        divisions (get subdivisions options
                       (let [s (Math/sqrt options)]
                         {:cols (Math/ceil s) :rows (Math/ceil s)}))]
    (->> (g/subdivide cell divisions)
         (map (fn [value piece]
                (let [tile (if (integer? value)
                             (nth tiles value)
                             value)]
                  (csvg/group (cond-> {:class "wfc-cell"}
                                on-click
                                (assoc :on-click (partial on-click loc value)))
                    (cell-tile tile piece))))
              values))))

(def rule-a
  (wfc/str->matrix
   "AAA
    ABA
    AAA"))

(def rule-b
  (wfc/str->matrix
   "AAAAA
    ABBBA
    ABCBA
    ABBBA
    AAAAA"))

(def rule-c
  (wfc/str->matrix
   "AAAAAA
    ABBBBA
    ABCCBA
    ABCCBA
    ABBBBA
    AAAAAA"))

(def rule-d
  (wfc/str->matrix
   "AAAAAAAA
    AAAAAAAA
    AABBBBAA
    AABCCBAA
    AABCCBAA
    AABBBBAA
    AAAAAAAA
    AAAAAAAA"))

(def rule-e
  (wfc/str->matrix
   "AAAAAAAAA
    AAABBBAAA
    AABBCBBAA
    AABCCCBBB
    AABCCCCCC
    AABCCCBBB
    AABBBBBAA
    AAAAAAAAA
    AAAAAAAAA"))

(defn scene
  [[width height]
   grid
   & {:keys [highlight tiles]
      :or {highlight #{} tiles []}
      :as args}]
  (let [[cols rows] (:dims grid)
        w (/ width cols)
        h (/ height rows)
        n-tiles (count tiles)]
    (csvg/svg {:width width
               :height height
               :stroke "none"
               :fill "none"}
      (for [j (range rows)
            i (range cols)]
        (let [loc (gv/vec2 i j)
              values (get grid loc)
              cell (rect/rect (* w i) (* h j) w h)
              changed? (contains? highlight loc)
              options (count values)]
          (csvg/group {:class "wfc-tile"
                       :stroke (if changed? "red" "none")}
            (if (<= options 16)
              (cell-subdivisions cell loc values args)
              (vary-meta cell assoc :fill
                         (csvg/hsl 0 0 (/ options n-tiles))))))))))

(defn generate-tileset [matrix rotations]
  (let [directions wfc/cardinal-directions
        pattern (wfc/matrix->grid matrix directions)
        tiles ((if rotations wfc/pattern->rotated-tiles
                   wfc/pattern->oriented-tiles)
               matrix 3)
        rules (wfc/adjacency-rules tiles)]
    (wfc/build-state
     {:dims [30 20]
      :directions directions
      :pattern pattern
      :tiles tiles
      :rules rules})))

(defn generate-cellset [matrix]
  (let [directions wfc/directions-8
        pattern (wfc/matrix->grid matrix directions)
        rules (wfc/rules pattern)
        tiles (vec (wfc/all-tiles rules))]
    (wfc/build-state
     {:dims [30 20]
      :directions directions
      :pattern pattern
      :tiles tiles
      :rules rules})))

(def modes {:cells generate-cellset
            :tileset generate-tileset})

(defn init-state [mode matrix rotations]
  {:highlight #{}
   :cancel nil
   :message nil
   :mode mode
   :rotations rotations
   :show-rules false
   :wfc-state ((get modes mode) matrix rotations)})

(defn reset [state]
  (cancel-active! state)
  (let [{:keys [mode rotations] {:keys [pattern]} :wfc-state} @state]
    (reset! state (init-state mode (wfc/grid->matrix pattern) rotations))))

(defn solve-step [state]
  (try
    (let [{:keys [changes] :as wfc-state} (wfc/solve-one (:wfc-state @state))]
      (swap! state assoc
             :message nil
             :wfc-state wfc-state
             :highlight changes)
      true)
    (catch :default e
      (cancel-active! state)
      (swap! state assoc :message e)
      false)))

(defn solve [state]
  (if-let [cancel (:cancel @state)]
    (async/close! cancel)
    (let [new-cancel (async/chan 1)]
      (swap! state assoc :cancel new-cancel)
      (go-loop [state state]
        (let [[_ c] (async/alts! [new-cancel (async/timeout 1)])]
          (if (and (not= c new-cancel) (solve-step state))
            (recur state)
            (swap! state assoc :cancel nil)))))))

(defn action-dispatch [state event]
  (case event
    :reset
    (fn [] (reset state))
    :solve
    (fn [] (solve state))
    :solve-one
    (fn []
      (cancel-active! state)
      (solve-step state))
    :pattern-edit-click
    (fn [loc _]
      (cancel-active! state)
      (swap! state update-in [:wfc-state :pattern loc] (partial cs/cycle-next ["A" "B" "C"]))
      (reset state))
    :pattern-clear
    (fn []
      (cancel-active! state)
      (swap! state update-in [:wfc-state :pattern]
             (fn [{[cols rows] :dims :as pattern}]
               (merge pattern
                      (into {}
                            (for [i (range cols)
                                  j (range rows)]
                              {(gv/vec2 i j) "A"})))))
      (reset state))
    :pattern-reset
    (fn []
      (cancel-active! state)
      (swap! state assoc-in [:wfc-state :pattern]
             (wfc/matrix->grid rule-e wfc/cardinal-directions))
      (reset state))
    :toggle-rotations
    (fn []
      (cancel-active! state)
      (swap! state update-in [:rotations] not)
      (reset state))
    :toggle-show-rules
    (fn []
      (swap! state update :show-rules not))))

(defn svg-tile [tile]
  (csvg/svg {:width 20
             :height 20
             :stroke "none"
             :fill "none"}
    [(cell-tile tile (rect/rect 20))]))

(def direction-name
  (zipmap wfc/directions-8
          [" N  " " E  " " S  " " W  "
           " NE " " SE " " SW " " NW "]))

(defn svg-adjacency [tile direction other]
  (let [d 20
        [x y] direction
        orient (cond (= 0 y) :horizontal
                     (= 0 x) :vertical
                     :else :diagonal)
        [w h] (case orient
                :horizontal [(* 2 d) d]
                :vertical [d (* 2 d)]
                :diagonal [(* 2 d) (* 2 d)])
        base (case orient
               :horizontal (gv/vec2 (if (> x 0) 0 d) 0)
               :vertical (gv/vec2 0 (if (> y 0) 0 d))
               :diagonal (gv/vec2 (if (> x 0) 0 d) (if (> y 0) 0 d)))
        r1 (g/translate (rect/rect d) base)
        r2 (g/translate
            (rect/rect d)
            (tm/+ base (tm/* direction d)))]
    (csvg/svg {:width w
               :height h
               :stroke "none"
               :fill "none"}
      [[:title (get direction-name direction)]
       (cell-tile tile r1)
       (cell-tile other r2)
       (vary-meta r2 assoc :opacity 0.25 :fill "white")])))

(defn tile-set [tiles]
  [:div
   [:h4 (str "Tiles (" (count tiles) ")")]
   [:div {:style {:column-count 12}}
    (for [[idx tile] (map-indexed vector tiles)]
      [:div {:key (str "ts-" idx)}
       (svg-tile tile)])]])

(defn rule-set [rules tiles show-rules emit]
  [:div
   [:h4 (str "Rules (" (count rules) ")  ")
    [:button.link {:on-click (emit :toggle-show-rules)}
     (if show-rules "(hide)" "(show)")]]
   (when show-rules
     [:div {:style {:column-count 8}}
      (let [simplified (sort-by first (group-by identity rules))
            numbered (some (fn [[_ e]] (> (count e) 1)) simplified)]
        (for [[idx [[tile-idx dir other-idx] examples]] (map-indexed vector simplified)]
          [:div {:key (str "rule-" idx)}
           (when numbered
             [:span (count examples) ": "])
           (svg-adjacency (nth tiles tile-idx) dir (nth tiles other-idx))]))])])

(defn display-patterns [state emit]
  (let [{:keys [wfc-state mode show-rules rotations]} state
        {:keys [pattern tiles rules]} wfc-state]
    [:div
     [:div.flexcols
      [:div
       [:h4 "Pattern"]
       (scene [150 150] pattern :on-click (emit :pattern-edit-click))]
      (when (= mode :tileset)
        [:div
         [:h4 "Settings"]
         [:div [:button {:on-click (emit :pattern-clear)} "Clear Pattern"]]
         [:div [:button {:on-click (emit :pattern-reset)} "Reset Pattern"]]
         [:div.label-set {:key "Include Rotations"}
          [:input {:type "checkbox" :checked rotations
                   :on-change (emit :toggle-rotations)}]
          [:label "Include Rotations"]]])
      [tile-set tiles]]
     [rule-set rules tiles show-rules emit]]))

(defn page []
  (let [state (ctrl/state (init-state :tileset rule-e true))
        emit (partial action-dispatch state)]
    (fn []
      (let [{:keys [wfc-state highlight cancel message]} @state
            {:keys [grid tiles]} wfc-state
            pattern-set (select-keys @state [:wfc-state :mode :show-rules :rotations])]
        [:div
         [:div.canvas-frame [scene [width height] grid
                             :tiles tiles
                             :highlight highlight
                             :on-click (partial set-cell! state)]]
         [:div#interface
          [:div.flexcols
           [:div [ctrl/change-mode state (keys modes) {:on-change (emit :reset)}]
            (when message
              [:div {:style {:color "red"}}
               (debug/pre-edn message)])]
           [:button.generate {:on-click (emit :reset)} "Reset"]
           [:button.generate {:on-click (emit :solve-one)} "Solve One"]
           [:button.generate {:on-click (emit :solve)} (if cancel "Stop" "Solve")]]
          [:p.readable
           "Click on a cell above to collapse it to a specific tile, or to
         expand it to the set of all legal tiles. Click on a cell in the pattern
         below to derive new tiles and rules."]
          [:p.readable "Does not yet support backtracking."]
          [display-patterns pattern-set emit]]]))))

(sketch/definition wave-function-collapse
  {:created-at "2022-04-26"
   :type :svg
   :tags #{}}
  (ctrl/mount page "sketch-host"))
