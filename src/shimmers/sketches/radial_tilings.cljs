(ns shimmers.sketches.radial-tilings
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn hexagons []
  (let [radius (* 0.95 height)
        revolutions 6
        hex-radius (/ radius (* 3 (+ revolutions 2.5)))
        hexes (mapv (fn [hex]
                      (assoc (hex/cube-hexagon hex hex-radius)
                             :coord hex
                             :ring (hex/cube-distance (gv/vec3) hex)))
                    (hex/cube-spiral (gv/vec3) revolutions))]
    (for [[idx {:keys [ring coord] :as hex}] (map-indexed vector hexes)]
      (let [poly (hex/flat-hexagon->polygon hex)]
        (csvg/group {}
          (vary-meta poly assoc :fill (csvg/hsl (* ring tm/PHI) 0.8 0.8 1.0))
          (csvg/center-label (:p hex)
                             (str idx "\n" coord)
                             {:font-size "0.5em"}))))))

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
