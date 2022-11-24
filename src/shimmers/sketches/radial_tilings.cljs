(ns shimmers.sketches.radial-tilings
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn hexagons []
  (let [radius (* 0.95 height)
        revolutions 4]
    (for [[idx hex] (map-indexed vector (hex/cube-spiral (gv/vec3) revolutions))]
      (let [circle (-> hex
                       (hex/cube-hexagon (/ radius (* 2 (+ revolutions 3.5)))))
            poly (hex/flat-hexagon->polygon circle)]
        (csvg/group {}
          poly
          (svg/text (:p circle)
                    (str idx)
                    {:font-weight "normal"
                     :font-size "0.66em"
                     :stroke "none"
                     :fill "black"
                     :alignment-baseline "middle"
                     :text-anchor "middle"}))))))

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
