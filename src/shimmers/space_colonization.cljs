(ns shimmers.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.framerate :as framerate]))

(defn setup []
  {:attractors (repeatedly 128 #(v/vec2 (+ 10 (q/random (- (q/width) 20)))
                                        (+ 10 (q/random (- (q/height) 50)))))})

(defn update-state [state]
  state)

(defn draw [{:keys [attractors]}]
  (q/background "black")
  (q/stroke "white")
  (doseq [p attractors]
    (apply q/point p)))

(defn ^:export run-sketch []
  (q/defsketch space-colonization
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


