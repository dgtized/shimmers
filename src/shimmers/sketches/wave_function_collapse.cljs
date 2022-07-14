(ns shimmers.sketches.wave-function-collapse
  (:require
   [cljs.core.async :as async :include-macros true]
   [clojure.set :as set]
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
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

(defn cell-set [state loc values]
  (when-let [cancel (:cancel @state)]
    (async/close! cancel))
  (let [{:keys [grid rules]} @state
        [legal-tiles _] (wfc/legal-at-location grid rules loc)
        [changes grid'] (wfc/propagate grid rules loc (set/intersection legal-tiles values))]
    (swap! state assoc
           :highlight (conj changes loc)
           :grid grid')))

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

(defn click [state loc value]
  (let [{:keys [tiles grid]} @state
        values (get grid loc)]
    (cell-set state loc
              (if (= (count values) 1)
                tiles
                #{value}))))

(defn grid->cells [[width height] grid
                   {:keys [highlight n-tiles on-click]
                    :or {highlight #{} n-tiles 10000}}]
  (let [[cols rows] (:dims grid)
        w (/ width cols)
        h (/ height rows)]
    (for [j (range rows)
          i (range cols)]
      (let [loc (gv/vec2 i j)
            values (get grid loc)
            cell (rect/rect (* w i) (* h j) w h)
            changed? (contains? highlight loc)
            options (count values)]
        (svg/group {:class "wfc-tile"
                    :stroke (if changed? "red" "none")}
                   (if (<= options 16)
                     (let [divisions
                           (get subdivisions options
                                (let [s (Math/sqrt options)]
                                  {:cols (Math/ceil s) :rows (Math/ceil s)}))]
                       (->> (g/subdivide cell divisions)
                            (map (fn [value piece]
                                   (svg/group (cond-> {:class "wfc-cell"}
                                                on-click
                                                (assoc :on-click (partial on-click loc value)))
                                              (cell-tile value piece)))
                                 values)))
                     (vary-meta cell assoc :fill
                                (csvg/hsl 0 0 (/ options n-tiles)))))))))

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
    AAAAAAAAA
    AABBBBBAA
    AABCCCBAA
    AABCCCBAA
    AABCCCBAA
    AABBBBBAA
    AAAAAAAAA
    AAAAAAAAA"))

(defn scene [[width height] grid & args]
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "none"}
            (grid->cells [width height] grid args)))

(defn generate-tileset [matrix]
  (let [directions wfc/cardinal-directions
        pattern (wfc/matrix->grid matrix directions)
        tiles (wfc/rules->rotated-tiles matrix 3)
        rules (wfc/adjacency-rules tiles)]
    {:grid (wfc/init-grid [30 20] directions (set tiles))
     :pattern pattern
     :tiles tiles
     :rules rules}))

(defn generate-cellset [matrix]
  (let [directions wfc/directions-8
        pattern (wfc/matrix->grid matrix directions)
        rules (wfc/rules pattern)
        tiles (wfc/all-tiles rules)]
    {:grid (wfc/init-grid [30 20] directions tiles)
     :pattern pattern
     :tiles tiles
     :rules rules}))

(def modes {:cells generate-cellset
            :tileset generate-tileset})

(defn init-state [mode matrix]
  (merge {:highlight #{}
          :cancel nil
          :mode mode
          :show-rules false}
         ((get modes mode) matrix)))

(defn reset [state]
  (when-let [cancel (:cancel @state)]
    (async/close! cancel))
  (reset! state (init-state (:mode @state) (wfc/grid->matrix (:pattern @state)))))

(defn solve [state]
  (if-let [cancel (:cancel @state)]
    (async/close! cancel)
    (let [new-cancel (async/chan 1)]
      (swap! state assoc :cancel new-cancel)
      (go-loop [state state]
        (let [[_ c] (async/alts! [new-cancel (async/timeout 1)])]
          (if-not (= c new-cancel)
            (let [{:keys [grid rules]} @state
                  [changes grid'] (wfc/solve-one grid rules)]
              (swap! state assoc
                     :grid grid'
                     :highlight changes)
              (if (seq changes)
                (recur state)
                (swap! state assoc :cancel nil)))
            (swap! state assoc :cancel nil)))))))

(defn solve-one [state]
  (when-let [cancel (:cancel @state)]
    (async/close! cancel))
  (let [{:keys [grid rules]} @state
        [changes grid'] (wfc/solve-one grid rules)]
    (swap! state assoc
           :grid grid'
           :highlight changes)))

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

(defn rule-set [rules show-rules toggle-rules]
  [:div
   [:h4 (str "Rules (" (count rules) ")  ")
    [:a {:on-click toggle-rules
         :href "javascript:void(0)"}
     (if show-rules "(hide)" "(show)")]]
   (when show-rules
     [:div {:style {:column-count 8}}
      (let [simplified (sort-by first (group-by identity rules))
            numbered (some (fn [[_ e]] (> (count e) 1)) simplified)]
        (for [[idx [[tile dir other] examples]] (map-indexed vector simplified)]
          [:div {:key (str "rule-" idx)}
           (when numbered
             [:span (count examples) ": "])
           (svg-adjacency tile dir other)]))])])

(defn display-patterns [state edit-click toggle-rules]
  (let [{:keys [pattern tiles rules show-rules]} state]
    [:div
     [:div.flexcols
      [:div
       [:h4 "Pattern"]
       (scene [150 150] pattern :on-click edit-click)]
      [tile-set tiles]]
     [rule-set rules show-rules toggle-rules]]))

(defn page []
  (let [state (ctrl/state (init-state :tileset rule-e))]
    (fn []
      (let [{:keys [grid highlight cancel]} @state
            pattern-set (select-keys @state [:pattern :tiles :rules :mode :show-rules])]
        [:div
         [:div.canvas-frame [scene [width height] grid
                             :highlight highlight
                             :n-tiles (count (:tiles @state))
                             :on-click (partial click state)]]
         [:div#interface
          [:div.flexcols
           [ctrl/change-mode state (keys modes) {:on-change #(reset state)}]
           [:button.generate {:on-click #(reset state)} "Reset"]
           [:button.generate {:on-click #(solve-one state)} "Solve One"]
           [:button.generate {:on-click #(solve state)} (if cancel "Stop" "Solve")]]
          [:p.readable
           "Click on a cell above to collapse it to a specific tile, or to
         expand it to the set of all legal tiles. Click on a cell in the pattern
         below to derive new tiles and rules."]
          [display-patterns pattern-set
           (fn [loc _]
             (swap! state update-in [:pattern loc] (partial cs/cycle-next ["A" "B" "C"]))
             (reset state))
           (fn []
             (swap! state update :show-rules not))]]]))))

(sketch/definition wave-function-collapse
  {:created-at "2022-04-26"
   :type :svg
   :tags #{}}
  (ctrl/mount page "sketch-host"))
