(ns shimmers.algorithm.flow-fields
  (:require
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.math.core :as tm]))

(defn noise-force [seed scale force]
  (fn [p]
    (v/polar force (* (dr/noise-at-point-01 seed scale p) eq/TAU))))

(defn flow [start-pt inside? next-pt lifespan]
  (let [path
        (->> start-pt
             (iterate next-pt)
             (take-while inside?)
             (take lifespan))]
    (when (> (count path) 1)
      path)))

(defn forward [start inside? force-fn lifespan]
  (flow start inside? (fn [p] (tm/+ p (force-fn p))) lifespan))

(defn backward [start inside? force-fn lifespan]
  (flow start inside? (fn [p] (tm/- p (force-fn p))) lifespan))

(defn bidirectional [start inside? force-fn lifespan]
  (let [behind (backward start inside? force-fn lifespan)
        ahead (forward start inside? force-fn lifespan)
        path (seq (concat (if (seq behind)
                            (reverse (rest behind))
                            [])
                          (if (seq ahead)
                            ahead
                            [])))]
    (when (> (count path) 1)
      path)))
