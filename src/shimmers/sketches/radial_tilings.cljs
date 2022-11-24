(ns shimmers.sketches.radial-tilings
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.hexagon :as hex]
   [thi.ng.geom.core :as g]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn hexagons []
  (let [radius (* 0.95 height)
        revolutions 4]
    (for [hex (hex/cube-spiral (gv/vec3) revolutions)]
      (-> hex
          (hex/cube-hexagon (/ radius (* 2 (+ revolutions 3.5))))
          hex/flat-hexagon->polygon))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 0.5}
     (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
       (hexagons)))))

(sketch/definition radial-tilings
  {:created-at "2022-11-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :radial-tilings)
              "sketch-host"))
