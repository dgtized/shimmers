(ns shimmers.sketches.polygrowth2
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.math.core :as tm]))

(defn as-polygon [[shape color]]
  (with-meta (g/as-polygon shape) {:stroke color}))

(defn grow-vertices [bounds shapes p factor polygon]
  (let [center (g/centroid polygon)]
    (as-> polygon it
      (g/vertices it)
      (dr/map-random-sample
       (constantly p)
       (fn [v] (let [v' (tm/+ center (tm/* (tm/- v center) factor))]
                (if (or (geometry/point-within? shapes v')
                        (not (geometry/point-within? bounds v')))
                  v v'))) it)
      (gp/polygon2 it)
      (with-meta it (meta polygon)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds [(cq/screen-rect 1.2)]
   :shapes (map as-polygon
                {(gc/circle (cq/rel-pos 0.35 0.65) 40) (color/hex->hsla "#3b4d61")
                 (gc/circle (cq/rel-pos 0.2 0.3) 30) (color/hex->hsla "#ef9d10")
                 (gc/circle (cq/rel-pos 0.75 0.4) 20) (color/hex->hsla "#3b4d61" 2.0)})})

(defn update-state [{:keys [bounds shapes] :as state}]
  (let [grow-by (partial grow-vertices bounds shapes 0.05 1.05)]
    (update state :shapes (partial map grow-by))))

(defn draw [{:keys [shapes]}]
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.05)
  (doseq [shape shapes
          :let [vertices (g/vertices shape)]]
    (q/no-fill)
    (q/stroke 0)
    (cq/color-if q/stroke (:stroke (meta shape)))
    (cq/draw-shape vertices)
    (q/fill 0)
    (doseq [v vertices]
      (cq/circle v 0.3))))

(defn page []
  (sketch/component
   :size [900 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition polygrowth2
    {:created-at "2021-05-02"
     :tags #{:deterministic}
     :type :quil}
  (ctrl/mount page))
