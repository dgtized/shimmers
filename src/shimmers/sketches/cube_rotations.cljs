(ns shimmers.sketches.cube-rotations
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.aabb :as aabb]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)

(defn projection [[w h] [x y z]]
  (let [perspective (* w 0.8)
        scale (/ perspective (+ perspective z))
        origin (tm/* (gv/vec2 w h) 0.5)]
    (-> (gv/vec2 x y)
        (tm/* scale)
        (tm/+ origin))))

(defn cubes [n]
  (let [perspective (partial projection [80 80])]
    (for [t (butlast (tm/norm-range n))
          :let [cube (-> (aabb/aabb 10)
                         g/center
                         (g/rotate-y (* (/ 1 8) eq/TAU))
                         (g/rotate-x 0.5)
                         (g/rotate-z (* t eq/TAU)))]]
      (for [[a b] (g/edges cube)]
        (gl/line2 (perspective a) (perspective b))))))

(defn grid []
  (let [margin 0
        r 8
        c 12
        bounds (rect/rect margin margin (- width margin) (- height margin))]
    (map (fn [bbox cube]
           (gu/fit-all-into-bounds (g/scale-size bbox 0.9) cube))
         (g/subdivide bounds {:cols c :rows r})
         (cubes (* r c)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.8}
            (grid)))

(sketch/definition cube-rotations
  {:created-at "2022-01-09"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :cube-rotations)
              "sketch-host"))
