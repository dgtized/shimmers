(ns shimmers.sketches.kd-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  {:points (repeatedly 32 (fn []
                            [(q/random (q/width))
                             (q/random (q/height))]))})

(defn draw [{:keys [points] :as state}]
  (q/background "white")
  (q/stroke-weight 2)
  (q/stroke "black")
  (doseq [point points]
    (apply q/point point)))

(defn ^:export run-sketch []
  (q/defsketch kd-tree
    :host "quil-host"
    :size [400 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))


