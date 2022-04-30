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
            cell (rect/rect (* w i) (* h j) w h)
            changed? (contains? highlight loc)]
        (->> (g/subdivide cell (get subdivisions (count values)))
             (map (fn [value piece]
                    (vary-meta piece assoc
                               :fill (get color-map value)
                               :on-click #(cell-set state loc (if (= (count values) 1)
                                                                tiles
                                                                #{value}))))
                  values)
             (svg/group {:stroke (if changed? "red" "none")}))))))

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
     :cancel nil
     :tiles tiles
     :rules rules}))

(defn reset [state]
  (when-let [cancel (:cancel @state)]
    (async/close! cancel))
  (reset! state (init-state)))

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

(defn page [state]
  (fn []
    (let [{:keys [grid highlight cancel]} @state]
      [:div
       [:div.canvas-frame [scene state grid highlight]]
       [:div#interface
        [:div.flexcols
         [:button.generate {:on-click #(reset state)} "Reset"]
         [:button.generate {:on-click #(solve-one state)} "Solve One"]
         [:button.generate {:on-click #(solve state)} (if cancel "Stop" "Solve")]]
        [:p.readable
         "Click on a cell to collapse it to a specific tile, or to expand it to
         the set of all legal tiles."]]])))

(sketch/definition wave-function-collapse
  {:created-at "2022-05-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (page (ctrl/state (init-state))) "sketch-host"))
