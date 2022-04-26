(ns shimmers.sketches.wave-function-collapse
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [thi.ng.geom.rect :as rect]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn grid->cells [grid]
  (let [[cols rows] (:dims grid)
        w (/ width cols)
        h (/ height rows)]
    (for [j (range rows)
          i (range cols)]
      (vary-meta (rect/rect (* w i) (* h j) w h)
                 merge {:fill (get {"A" "#000000"
                                    "B" "#ffffff"
                                    "C" "#888888"}
                                   (first (get grid (gv/vec2 i j))))}))))

(defn shapes []
  (let [rt (wfc/rules (wfc/matrix->grid wfc/rule-a) wfc/cardinal-directions)]
    (grid->cells (wfc/solve (wfc/init-grid [32 24] (wfc/all-tiles rt))
                            rt))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition wave-function-collapse
  {:created-at "2022-05-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :wave-function-collapse)
              "sketch-host"))
