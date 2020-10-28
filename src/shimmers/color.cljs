(ns shimmers.color
  (:require [quil.core :as q :include-macros true]
            [thi.ng.color.core :as col]
            [thi.ng.color.gradients :as grad]))

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

(defn as-vector [color]
  (map (fn [x] (float (* x 255)))
       [(col/red color) (col/green color) (col/blue color) (col/alpha color)]))

(defn random-gradient [scheme]
  (let [color (->> scheme
                   grad/cosine-schemes
                   (grad/cosine-gradient 32)
                   rand-nth)]
    (as-vector (col/adjust-alpha color -0.95))))
