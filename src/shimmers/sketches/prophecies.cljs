(ns shimmers.sketches.prophecies
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn stem [base height angle]
  (let [connect (v/+polar base height angle)]
    [(gl/line2 base connect)
     (rect/rect (tm/+ connect (gv/vec2 (* -0.5 height) (* 1.0 height)))
                (tm/+ connect (gv/vec2 (* +0.5 height) 0)))]))

(defn shapes []
  (let [c1 (gc/circle (rv 0.35 0.5) (* width 0.25))
        c2 (gc/circle (rv 0.75 0.5) (* width 0.15))
        meridian (g/scale-size (gl/line2 (:p c1) (:p c2)) 1.75)
        [p q] (g/vertices meridian)
        heading (g/heading (tm/- q p))]
    (concat [c1 c2 meridian]
            (stem (g/point-at meridian 0.33) (* width 0.05) (+ tm/HALF_PI heading))
            (stem (g/point-at meridian 0.20) (* width -0.05) (+ tm/HALF_PI heading)))))

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
