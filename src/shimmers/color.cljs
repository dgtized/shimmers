(ns shimmers.color
  (:require [quil.core :as q :include-macros true]))

(defn random []
  (let [colors [[128 192 128 32]
                [128 0 0 32]
                [0 128 0 32]
                [0 0 128 32]
                [0 0 192 32]]]
    (rand-nth colors)))

(defn random-lerp [from to]
  (let [color (q/lerp-color from to (rand))]
    [(q/red color) (q/green color) (q/blue color) 32]))
