(ns shimmers.math.probability
  (:require [shimmers.common.sequence :as cs]))

(defn chance [p]
  (< (rand) p))

;; Modified from https://github.com/clojure/data.generators/blob/master/src/main/clojure/clojure/data/generators.clj#L73
;; as it was not available for Clojurescript
(defn weighted
  "Given a map of generators and weights, return a value from one of
   the generators, selecting generator based on weights."
  [m]
  (let [weights (reductions + (vals m))
        total   (last weights)
        choices (map vector (keys m) weights)
        choice  (* total (rand))]
    (loop [[[c w] & more] choices]
      (when w
        (if (< choice w)
          c
          (recur more))))))

(defn weighted-by [f xs]
  (weighted (cs/mapping f xs)))

(comment
  (weighted {:a 0.2 :b 0.8})
  (weighted-by inc [1 2 3]))
