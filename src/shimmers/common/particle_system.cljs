(ns shimmers.common.particle-system
  (:require
   [quil.core :as q :include-macros true]
   [shimmers.math.vector :as v]))

(defn step [{:keys [position velocity acceleration] :as particle}]
  (let [new-velocity (v/add velocity acceleration)
        new-position (v/add position new-velocity)]
    (assoc particle
           :last-pos position
           :position new-position
           :velocity new-velocity
           :acceleration acceleration)))

(defn draw [particles & {:keys [weight]}]
  (doseq [{:keys [position last-pos color] :as particle} particles]
    (let [[lx ly] last-pos
          [x y] position]
      (when color
        (apply q/stroke color))
      (when weight
        (q/stroke-weight (weight particle)))
      (q/line lx ly x y))))

