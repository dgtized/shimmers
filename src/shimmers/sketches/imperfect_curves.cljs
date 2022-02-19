(ns shimmers.sketches.imperfect-curves
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; original plan was a perspective drawing of two building faces joined by a curve

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn upper-ellipse [p rx ry]
  (for [t (range tm/PI tm/TWO_PI 0.1)]
    (tm/+ (gv/vec2 (* rx (Math/cos t))
                   (* ry (Math/sin t)))
          p)))

(defn shapes []
  (for [t (dr/var-range 10)]
    (gl/linestrip2 (upper-ellipse (rv 0.5 0.5) (* t 0.3 width) (* t 0.4 height)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition imperfect-curves
  {:created-at "2022-02-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :imperfect-curves)
              "sketch-host"))
