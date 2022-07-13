(ns shimmers.sketches.wave-function-collapse
  (:require
   [cljs.core.async :as async :include-macros true]
   [clojure.set :as set]
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv])
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

(defn grid->cells [[width height] state grid highlight]
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
                                                state
                                                (assoc :on-click
                                                       #(cell-set state loc
                                                                  (if (= (count values) 1)
                                                                    (:tiles @state)
                                                                    #{value}))))
                                              (cell-tile value piece)))
                                 values)))
                     (vary-meta cell assoc :fill
                                (csvg/hsl 0 0 (/ options (count (:tiles @state)))))))))))

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

(defn scene [[width height] state grid highlight]
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "none"}
            (grid->cells [width height] state grid highlight)))

(defn from-tileset []
  (let [matrix rule-e
        pattern (wfc/matrix->grid matrix wfc/cardinal-directions)
        tiles (wfc/rules->rotated-tiles matrix 3)
        rules (wfc/adjacency-rules tiles)]
    {:grid (wfc/init-grid [30 20] wfc/cardinal-directions (set tiles))
     :pattern pattern
     :tiles tiles
     :rules rules}))

(defn from-cells []
  (let [directions wfc/directions-8
        pattern (wfc/matrix->grid rule-d directions)
        rules (wfc/rules pattern)
        tiles (wfc/all-tiles rules)]
    {:grid (wfc/init-grid [30 20] directions tiles)
     :pattern pattern
     :tiles tiles
     :rules rules}))

(def modes {:cells from-cells
            :tileset from-tileset})

(defn init-state [mode]
  (merge {:highlight #{}
          :cancel nil
          :mode mode}
         ((get modes mode))))

(defn reset [state]
  (when-let [cancel (:cancel @state)]
    (async/close! cancel))
  (reset! state (init-state (:mode @state))))

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

(defn tile-set [tiles]
  [:div
   [:h4 (str "Tiles (" (count tiles) ")")]
   [:div {:style {:column-count 8}}]
   (for [[idx tile] (map-indexed vector tiles)]
     [:span {:key (str "ts-" idx) :style {:margin-right "5px"}}
      (svg-tile tile)])])

(def direction-name
  (zipmap wfc/directions-8
          [" N  " " E  " " S  " " W  "
           " NE " " SE " " SW " " NW "]))

(defn rule-set [rules]
  [:div
   [:h4 (str "Rules (" (count rules) ")")]
   [:div {:style {:column-count 8}}
    (for [[idx [tile dir other]] (map-indexed vector rules)]
      [:div {:key (str "rule-" idx)}
       (svg-tile tile)
       [:code (get direction-name dir (str " unknown " dir))]
       (svg-tile other)])]])

(defn pattern-editor [pattern]
  [:div
   [:h4 "Pattern"]
   (scene [150 150] nil pattern #{})])

(defn page []
  (let [state (ctrl/state (init-state :cells))]
    (fn []
      (let [{:keys [grid highlight cancel]} @state]
        [:div
         [:div.canvas-frame [scene [width height] state grid highlight]]
         [:div#interface
          [:div.flexcols
           [ctrl/change-mode state (keys modes) {:on-change #(reset state)}]
           [:button.generate {:on-click #(reset state)} "Reset"]
           [:button.generate {:on-click #(solve-one state)} "Solve One"]
           [:button.generate {:on-click #(solve state)} (if cancel "Stop" "Solve")]]
          [:p.readable
           "Click on a cell to collapse it to a specific tile, or to expand it to
         the set of all legal tiles."]
          (let [{:keys [pattern tiles rules]} @state]
            [:div
             [:div.flexcols
              [pattern-editor pattern]
              [tile-set tiles]]
             [rule-set rules]])]]))))

(sketch/definition wave-function-collapse
  {:created-at "2022-04-26"
   :type :svg
   :tags #{}}
  (ctrl/mount page "sketch-host"))
