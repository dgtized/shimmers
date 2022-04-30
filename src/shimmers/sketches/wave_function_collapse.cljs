(ns shimmers.sketches.wave-function-collapse
  (:require
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def color-map
  {"A" "#4444dd"
   "B" "#cdcd00"
   "C" "#228b22"})

(defn cell-reset [state loc]
  (fn []
    (swap! state assoc-in [:grid loc] (:tiles @state))))

(defn grid->cells [state grid]
  (let [[cols rows] (:dims grid)
        w (/ width cols)
        h (/ height rows)]
    (for [j (range rows)
          i (range cols)]
      (let [loc (gv/vec2 i j)
            values (get grid loc)
            id (+ (* j cols) i)
            cell (rect/rect (* w i) (* h j) w h)]
        (if (= (count values) 1)
          (vary-meta cell assoc
                     :id id
                     :on-click (cell-reset state loc)
                     :fill (get color-map (first values)))
          (->> (g/subdivide cell {:cols 2 :rows 2})
               (map (fn [color piece] (vary-meta piece assoc :fill color))
                    (map color-map values))
               (svg/group {:id id})))))))

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

(defn scene [state grid]
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "none"}
            (apply list (grid->cells state grid))))

(defn init-state []
  (let [directions wfc/directions-8+4
        rules (wfc/rules (wfc/matrix->grid rule-c directions))
        tiles (wfc/all-tiles rules)]
    {:grid (wfc/init-grid [40 30] directions tiles)
     :tiles tiles
     :rules rules}))

(defn reset [state]
  (reset! state (init-state)))

(defn solve [state]
  (let [{:keys [rules]} @state]
    (swap! state update :grid wfc/solve rules)))

(defn page [state]
  (fn []
    (let [{:keys [grid]} @state]
      [:div
       [:div.canvas-frame [scene state grid]]
       [:div#interface
        [:div.flexcols
         [:button.generate {:on-click #(reset state)} "Reset"]
         #_[:button.generate {} "Step"]
         [:button.generate {:on-click #(solve state)} "Solve"]]]])))

(sketch/definition wave-function-collapse
  {:created-at "2022-05-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (page (ctrl/state (init-state))) "sketch-host"))
