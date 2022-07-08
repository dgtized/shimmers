(ns shimmers.sketches.prophecies
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.line :as gl]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [c1 (gc/circle (rv 0.35 0.5) (* width 0.25))
        c2 (gc/circle (rv 0.75 0.5) (* width 0.15))
        meridian (g/scale-size (gl/line2 (:p c1) (:p c2)) 1.75)
        [p q] (g/vertices meridian)
        heading (g/heading (tm/- q p))]
    [c1 c2
     meridian
     (let [b (g/point-at meridian 0.33)]
       (gl/line2 b (v/+polar b (* width 0.05) (+ tm/HALF_PI heading))))]))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (shapes)))

(sketch/definition prophecies
  {:created-at "2022-07-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :prophecies)
              "sketch-host"))
