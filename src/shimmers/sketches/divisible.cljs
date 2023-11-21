(ns shimmers.sketches.divisible
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.geometry.bounded-shapes :as bounded]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn generate-boxes [bounds]
  (iterate (fn [existing]
             (let [candidate (bounded/rectangle bounds
                                                (tm/roundto (dr/random 0.05 0.3) 0.05)
                                                (tm/roundto (dr/random 0.05 0.3) 0.05))
                   margin (g/scale-size candidate 1.4)]
               (if (some (fn [box] (when (collide/overlaps? margin box) box))
                         existing)
                 existing
                 (conj existing candidate))))
           []))

(defn shapes [bounds]
  (first (drop-while (fn [s] (< (count s) 11))
                     (generate-boxes bounds))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height))))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :divisible]
     [:div.readable-width]]))

(sketch/definition divisible
    {:created-at "2023-11-21"
     :tags #{}
     :type :svg}
  (ctrl/mount (page)))
