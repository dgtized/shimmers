(ns shimmers.algorithm.flow-fields
  (:require
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn noise-force [seed scale force]
  (fn [p]
    (v/polar force (* (dr/noise-at-point-01 seed scale p) eq/TAU))))

(defn flow-path [bounds start-fn force-fn lifespan]
  (let [path
        (->> (start-fn)
             (iterate (fn [p] (tm/+ p (force-fn p))))
             (take (lifespan))
             (take-while (fn [p] (g/contains-point? bounds p))))]
    (when (and (seq path) (> (count path) 1))
      path)))
