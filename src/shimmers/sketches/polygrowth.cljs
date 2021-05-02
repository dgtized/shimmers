(ns shimmers.sketches.polygrowth
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.math.core :as tm]))

(defn grow [polygon]
  (let [center (geom/centroid polygon)]
    (gp/polygon2
     (for [v (geom/vertices polygon)]
       (tm/+ center (tm/* (tm/- v center) 1.015))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(geom/as-polygon (gc/circle (cq/rel-pos 0.5 0.5) 30))
            (geom/as-polygon (gc/circle (cq/rel-pos 0.2 0.3) 30))
            (geom/as-polygon (gt/triangle2 (cq/rel-pos 0.3 0.8) (cq/rel-pos 0.4 0.9) (cq/rel-pos 0.45 0.8)))]})

(defn update-state [state]
  (update state :shapes (partial map grow)))

(defn draw [{:keys [shapes]}]
  (doseq [shape shapes
          :let [vertices (geom/vertices shape)]]
    (q/no-fill)
    (cq/draw-shape vertices)
    (q/fill 0)
    (doseq [[x y] vertices]
      (q/ellipse x y 1.0 1.0))))

(defn ^:export run-sketch []
  ;; 20210502
  (q/defsketch polygrowth
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
