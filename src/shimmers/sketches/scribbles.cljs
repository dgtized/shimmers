(ns shimmers.sketches.scribbles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]))

(defn scribble [points]
  (q/stroke-weight 0.4)
  (q/curve-tightness 2.5)
  (q/begin-shape)
  (doseq [p points]
    (q/curve-vertex (cq/rel-w (:x p)) (cq/rel-h (:y p))))
  (q/end-shape))

(defn random-vertex []
  (gv/vec2 (rand) (rand)))

(defn setup []
  (q/frame-rate 1)
  {:points (repeatedly 8 random-vertex)})

(defn update-state [state]
  {:points (repeatedly 8 random-vertex)})

(defn draw [{:keys [points]}]
  (q/background 255)
  (scribble points))

(defn ^:export run-sketch []
  ;; 20210309
  (q/defsketch scribbles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
