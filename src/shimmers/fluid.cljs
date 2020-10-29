(ns shimmers.fluid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.vector :as v]))

(defn setup []
  (q/background "black")
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch fluid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))
