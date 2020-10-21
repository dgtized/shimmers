(ns shimmers.particles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-particle []
  {:x (q/random (q/width))
   :y (q/random (q/height))})

(defn setup []
  (q/background "white")
  {:particles (repeatedly 100 make-particle)})

(defn update-state [{:keys [particles]}]
  {:particles (repeatedly 100 make-particle)})

(defn draw [{:keys [particles]}]
  (doseq [{:keys [x y] :as particle} particles]
    (q/point x y)))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [500 500]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


