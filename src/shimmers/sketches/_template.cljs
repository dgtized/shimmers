(ns shimmers.sketches.template
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defn setup []
  {})

(defn update-state [state]
  state)

(defn draw [state]
  state)

(defn ^:export run-sketch []
  (q/defsketch template
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
