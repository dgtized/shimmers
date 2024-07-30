(ns shimmers.sketches.designed-imperfections
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [seed (dr/noise-seed)]
    (for [box (g/subdivide (csvg/screen width height)
                           {:cols (/ width 10) :rows (/ height 10)})]
      (let [pos (g/centroid box)
            n (dr/noise-at-point seed 0.015 pos)
            damp (/ 1.0 (Math/pow 1.004 (g/dist pos (rv 0.5 0.5))))]
        (gc/circle (tm/+ pos
                         (v/polar (* 8.0 damp) (* eq/TAU n)))
                   (* 2.0 (+ 1.0 (* 0.5 (Math/sin (* eq/TAU n)) damp))))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes)))

(sketch/definition designed-imperfections
  {:created-at "2024-07-28"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
