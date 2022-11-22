(ns shimmers.sketches.deeply-askew
  (:require
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 1000)
(def height 700)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [bounds (g/scale-size (csvg/screen width height) 0.9)
        seed (gv/vec2 (dr/random 100) (dr/random 100))
        point-noise (fn [p] (tm/clamp (Math/pow (dr/noise-at-point seed 0.005 p) 2) 0.1 1.0))
        points (pds/generate-dynamic bounds 15 [6 160] point-noise)
        cells (delvor/voronoi-cells points bounds)]
    (->> cells
         (mapcat (fn [cell]
                   (let [centered (g/center cell)
                         centroid (g/centroid cell)
                         sign (dr/rand-nth [1 -1])
                         drift (v/polar (* 4 (point-noise (tm/+ centroid (gv/vec2 100 100))))
                                        (* eq/TAU (point-noise centroid)))]
                     (mapv (fn [scale]
                             (-> centered
                                 (g/scale-size (- 1.0 scale))
                                 (g/rotate (* sign 0.66 scale))
                                 (g/translate (tm/+ centroid (tm/* drift scale)))))
                           (range 0.03 0.9 0.15))))))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"
              :stroke-width 1.0}
     (shapes))))

(sketch/definition deeply-askew
  {:created-at "2022-11-14"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :deeply-askew)
              "sketch-host"))
