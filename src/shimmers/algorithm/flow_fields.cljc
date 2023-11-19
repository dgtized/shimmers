(ns shimmers.algorithm.flow-fields
  (:require
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn flow-path [bounds start-fn seed scale force lifespan]
  (let [path
        (->> (start-fn)
             (iterate
              (fn [p]
                (let [noise (dr/noise-at-point-01 seed scale p)]
                  (tm/+ p (v/polar force (* noise eq/TAU))))))
             (take (lifespan))
             (take-while (fn [p] (g/contains-point? bounds p))))]
    (when (and (seq path) (> (count path) 1))
      path)))
