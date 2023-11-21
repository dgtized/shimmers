(ns shimmers.sketches.divisible
  (:require
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.bounded-shapes :as bounded]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
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

;; this doesn't work because a punch might intersect with more than one rectangle
(defn punch-out [rect punch]
  (if (collide/bounded? rect punch)
    (square/difference rect punch)
    [rect]))

(defn shapes [bounds]
  (let [punches (first (drop-while (fn [s] (< (count s) 5))
                                   (generate-boxes bounds)))]
    (concat (reduce (fn [rects box]
                      (mapcat (fn [r] (punch-out r box)) rects))
                    [bounds]
                    (sort-by (fn [s] (g/dist (g/centroid s) (g/centroid bounds))) punches))
            (map (fn [s] (vary-meta s assoc :stroke "red")) punches))))

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
     [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
      [view-sketch/generate :divisible]]
     [:div.readable-width "Experimenting with dividing a rectangle by punching
     out a set of rectangles inside of it, and then calculating the set of
     rectangles remaining which would tile the space."]]))

(sketch/definition divisible
    {:created-at "2023-11-21"
     :tags #{}
     :type :svg}
  (ctrl/mount (page)))
