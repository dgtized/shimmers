(ns shimmers.sketches.polygrowth
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.math.core :as tm]))

(defn grow-clipped [bounds shapes factor polygon]
  (let [center (g/centroid polygon)]
    (-> (for [v (g/vertices polygon)]
          (let [v' (tm/+ center (tm/* (tm/- v center) factor))]
            (if (or (geometry/point-within? shapes v')
                    (not (geometry/point-within? bounds v')))
              v v')))
        gp/polygon2
        (with-meta (meta polygon)))))

(defn as-polygon [[shape color]]
  (with-meta (g/as-polygon shape) {:stroke color}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds [(cq/screen-rect 4)]
   :shapes (map as-polygon
                {(gc/circle (cq/rel-pos 0.4 0.5) 40) (color/hex->hsla "#3b4d61")
                 (gc/circle (cq/rel-pos 0.2 0.3) 30) (color/hex->hsla "#ef9d10")
                 (gt/triangle2 (cq/rel-pos 0.3 0.8)
                               (cq/rel-pos 0.4 0.9)
                               (cq/rel-pos 0.45 0.8))
                 (color/hex->hsla "#6b7b8c")})})

(defn update-state [{:keys [shapes bounds] :as state}]
  (let [grow-by (partial grow-clipped bounds shapes 1.02)]
    (update state :shapes (partial map grow-by))))

(defn draw [{:keys [shapes]}]
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.75)
  (doseq [shape shapes
          :let [vertices (g/vertices shape)]]
    (q/no-fill)
    (q/stroke 0)
    (cq/color-if q/stroke (:stroke (meta shape)))
    (cq/draw-shape vertices)
    (q/fill 0)
    (doseq [v vertices]
      (cq/circle v 0.5))))

(defn page []
  (sketch/component
   :size [900 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition polygrowth
  {:created-at "2021-05-02"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
