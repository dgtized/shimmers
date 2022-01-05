(ns shimmers.sketches.deeper-squares
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn inset-rect [rect scale]
  (let [[a _ c _] (gp/inset-polygon (g/vertices rect) scale)]
    (rect/rect a c)))

(defn deepen [square]
  (for [s (range 0 20)]
    (inset-rect square (* s (dr/random -11 -7)))))

(defn shapes []
  (deepen (rect/rect (rv 0.2 0.1) (rv 0.8 0.9))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition deeper-squares
  {:created-at "2022-01-04"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :deeper-squares)
              "sketch-host"))
