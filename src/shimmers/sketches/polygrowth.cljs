(ns shimmers.sketches.polygrowth
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(geom/as-polygon (gc/circle (cq/rel-pos 0.5 0.5) 50))]})

(defn update-state [state]
  state)

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
