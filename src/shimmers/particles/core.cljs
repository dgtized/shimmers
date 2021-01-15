(ns shimmers.particles.core
  (:require [quil.core :as q :include-macros true]))

(defn draw [particles & {:keys [weight]}]
  (doseq [{:keys [position last-pos color] :as particle} particles]
    (let [[lx ly] last-pos
          [x y] position]
      (when color
        (apply q/stroke color))
      (when weight
        (q/stroke-weight (weight particle)))
      (q/line lx ly x y))))

