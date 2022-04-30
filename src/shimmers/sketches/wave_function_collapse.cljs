(ns shimmers.sketches.wave-function-collapse
  (:require
   [clojure.set :as set]
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def color-map
  {"A" "#4444dd"
   "B" "#cdcd00"
   "C" "#228b22"})

(defn cell-set [state loc values]
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
                   9 {:cols 3 :rows 3}})

(defn grid->cells [state grid highlight]
  (let [[cols rows] (:dims grid)
        tiles (:tiles @state)
        w (/ width cols)
        h (/ height rows)]
    (for [j (range rows)
          i (range cols)]
      (let [loc (gv/vec2 i j)
            values (get grid loc)
            id (+ (* j cols) i)
            cell (rect/rect (* w i) (* h j) w h)
            changed? (contains? highlight loc)]
        (if (= (count values) 1)
          (vary-meta cell assoc
                     :id id
                     :on-click #(cell-set state loc tiles)
                     :stroke (if changed? "red" "none")
                     :fill (get color-map (first values)))
          (->> (g/subdivide cell (get subdivisions (count values)))
               (map (fn [value piece]
                      (vary-meta piece assoc
                                 :fill (get color-map value)
                                 :on-click #(cell-set state loc #{value})))
                    values)
               (svg/group {:id id
                           :stroke (if changed? "red" "none")})))))))

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

(defn scene [state grid highlight]
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "none"}
            (apply list (grid->cells state grid highlight))))

(defn init-state []
  (let [directions wfc/directions-8
        rules (wfc/rules (wfc/matrix->grid rule-c directions))
        tiles (wfc/all-tiles rules)]
    {:grid (wfc/init-grid [30 20] directions tiles)
     :highlight #{}
     :tiles tiles
     :rules rules}))

(defn reset [state]
  (reset! state (init-state)))

(defn solve [state]
  (let [{:keys [rules]} @state]
    (swap! state assoc :highlight #{})
    (swap! state update :grid wfc/solve rules)))

(defn page [state]
  (fn []
    (let [{:keys [grid highlight]} @state]
      [:div
       [:div.canvas-frame [scene state grid highlight]]
       [:div#interface
        [:div.flexcols
         [:button.generate {:on-click #(reset state)} "Reset"]
         #_[:button.generate {} "Step"]
         [:button.generate {:on-click #(solve state)} "Solve"]]
        [:p.readable
         "Click on a cell to collapse it to a specific tile, or to expand it to
         the set of all legal tiles."]]])))

(sketch/definition wave-function-collapse
  {:created-at "2022-05-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (page (ctrl/state (init-state))) "sketch-host"))
