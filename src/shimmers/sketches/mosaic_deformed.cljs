(ns shimmers.sketches.mosaic-deformed
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.polygon :as gp]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def radius (* 0.45 height))

(defn ring [seed r displace]
  (for [t (dr/gaussian-range 0.01 0.01)]
    (let [p (v/polar (* radius r ) (* t eq/TAU))
          noise (dr/noise-at-point-01 seed 0.003 p)]
      (tm/+ p (v/polar displace (* eq/TAU noise))))))

(defn shapes [seed]
  (mapv (fn [r] (gp/polygon2 (vec (ring seed (- r 0.025) (* 0.025 radius (+ 1 r))))))
        (dr/gaussian-range 0.1 0.005)))

(defn scene [seed]
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes seed))))

(defn page []
  (let [seed (gv/vec2 (dr/random 100) (dr/random 100))]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene seed]]
       [:div.flexcols
        [view-sketch/generate :mosaic-deformed]]])))

(sketch/definition mosaic-deformed
  {:created-at "2023-10-04"
   :tags #{}
   :type :svg}
  (ctrl/mount (page)))
