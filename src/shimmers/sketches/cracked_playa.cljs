(ns shimmers.sketches.cracked-playa
  (:require
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.math.noise :as noise]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn noise-at-point [p]
  (let [[x y] (tm/* p 0.005)]
    (tm/clamp01 (noise/noise2 x y))))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        points (pds/generate-dynamic bounds 10 [25 60] noise-at-point)]
    (for [p points]
      (gc/circle p 2))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition cracked-playa
  {:created-at "2022-04-03"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :cracked-playa)
              "sketch-host"))
