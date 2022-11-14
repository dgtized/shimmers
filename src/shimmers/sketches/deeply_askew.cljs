(ns shimmers.sketches.deeply-askew
  (:require
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [bounds (g/scale-size (rect/rect 0 0 width height) 0.9)
        seed (gv/vec2 (dr/random 100) (dr/random 100))
        points (pds/generate-dynamic bounds 10 [32 96] (partial dr/noise-at-point seed 0.1))
        points (remove (fn [_] (dr/chance 0.8)) points)
        cells (delvor/voronoi-cells points bounds)]
    (->> cells
         (mapcat (fn [cell]
                   (let [centered (g/center cell)
                         centroid (g/centroid cell)
                         sign (dr/rand-nth [1 -1])]
                     (mapv (fn [scale]
                             (-> centered
                                 (g/scale-size (- 1.0 scale))
                                 (g/rotate (* sign 0.5 scale))
                                 (g/translate centroid)))
                           (range 0.03 0.9 0.15))))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0}
    (shapes)))

(sketch/definition deeply-askew
  {:created-at "2022-11-14"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :deeply-askew)
              "sketch-host"))
