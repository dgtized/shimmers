(ns shimmers.cube
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.framerate :as framerate]))

(defn setup []
  (q/background "white")
  {:vertices [(v/vec2 100 100) (v/vec2 200 100)
              (v/vec2 200 200) (v/vec2 100 200)]})

(defn update-state [state]
  state)

(defn draw [{:keys [vertices]}]
  (q/stroke "black")
  (q/stroke-weight 1)
  (q/begin-shape)
  (doseq [vertex vertices]
    (apply q/vertex vertex))
  (apply q/vertex (first vertices))
  (q/end-shape)
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch cube
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


