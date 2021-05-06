(ns shimmers.sketches.shattered-boxes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.rect :as rect]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 0.95)]})

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.5)
  (doseq [shape shapes]
    (cq/draw-shape (geom/vertices shape))))

(defn ^:export run-sketch []
  ;; 20210505
  (q/defsketch shattered-boxes
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
