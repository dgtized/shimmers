(ns shimmers.particles.core
  (:require [quil.core :as q :include-macros true]))

(defn draw [particles & {:keys [weight]}]
  (doseq [{:keys [position last-pos color] :as particle} particles]
    (apply q/stroke color)
    (let [[lx ly] last-pos
          [x y] position]
      (when weight
        (q/stroke-weight (weight particle)))
      (q/line lx ly x y))))

