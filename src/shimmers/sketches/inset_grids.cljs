(ns shimmers.sketches.inset-grids
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.core :as g]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def min-area (* 0.0015 width height))

(defn splits [{[w h] :size}]
  (if (> w h)
    {:rows (dr/random-int 1 5)
     :cols (dr/random-int 2 11)}
    {:rows (dr/random-int 2 11)
     :cols (dr/random-int 1 5)}))

(defn division [limit bounds]
  (let [rect (g/scale-size bounds 0.975)]
    (if (< (g/area rect) limit)
      [rect]
      (mapcat (fn [r]
                (into [r] (division limit (g/scale-size r 0.975))))
              (g/subdivide rect (splits rect))))))

(defn shapes [bounds]
  (division min-area bounds))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.66}
    (shapes (csvg/screen width height))))

(sketch/definition inset-grids
  {:created-at "2024-07-16"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
