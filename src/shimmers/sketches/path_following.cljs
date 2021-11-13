(ns shimmers.sketches.path-following
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.line :as gl]))

(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(def original
  (-> [(r 0.1 0.5) (r 0.3 0.3) (r 0.6 0.6) (r 0.9 0.5)]
      bezier/auto-spline2
      (g/sample-uniform 10.0 true)
      gl/linestrip2))

(defn scene []
  (let [offset (g/translate original (r 0.0 -0.1))]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 5.0}
              (svg/polyline (:points original) {:stroke "blue" :key "a1"})
              (svg/polyline (:points offset) {:key "a2"}))))

(defn page []
  [:div (scene)])

(sketch/definition path-following
  {:created-at "2021-11-12"
   :type :svg
   :tags #{:demo}}
  (ctrl/mount page "canvas-host"))
