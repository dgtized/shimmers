(ns shimmers.algorithm.flow-fields
  (:require
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.math.core :as tm]))

(defn noise-force [seed scale force]
  (fn [p]
    (v/polar force (* (dr/noise-at-point-01 seed scale p) eq/TAU))))

(defn forward [start inside? force-fn lifespan]
  (let [path
        (->> start
             (iterate (fn [p] (tm/+ p (force-fn p))))
             (take (lifespan))
             (take-while inside?))]
    (when (and (seq path) (> (count path) 1))
      path)))

(defn backward [start inside? force-fn lifespan]
  (let [path
        (->> start
             (iterate (fn [p] (tm/- p (force-fn p))))
             (take (lifespan))
             (take-while inside?))]
    (when (and (seq path) (> (count path) 1))
      path)))

(defn bidirectional [start inside? force-fn lifespan]
  (let [behind (backward start inside? force-fn lifespan)
        ahead (forward start inside? force-fn lifespan)
        path (seq (concat (if (seq behind)
                            (reverse (rest behind))
                            [])
                          (if (seq ahead)
                            ahead
                            [])))]
    (when (and (seq path) (> (count path) 1))
      path)))
