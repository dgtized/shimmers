(ns shimmers.math.probability
  (:require [shimmers.common.sequence :as cs]
            [kixi.stats.distribution :as ksd]
            [thi.ng.math.core :as tm]))

(defn chance [prob]
  (< (rand) prob))

(defn happensity
  "Returns a 0 with probability `prob`, otherwise evenly distributes"
  [prob]
  (if (chance prob)
    (rand)
    0))

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

(defn map-random-sample
  "Apply `f` to the subset of `coll` selected with probability `prob` with the
  unsampled elements intermingled as before."
  [prob f coll]
  (map (fn [x] (if (chance prob) (f x) x)) coll))

(comment (map-random-sample 0.1 inc (range 10)))

(defn gaussian-clamped [mean sd]
  (let [dist (ksd/normal {:mu mean :sd sd})]
    (fn []
      (-> dist
          ksd/draw
          (tm/clamp 0 1)))))

;; https://stats.stackexchange.com/questions/481543/generating-random-points-uniformly-on-a-disk
(defn confusion-disk [[x y] r]
  (let [radius (Math/sqrt (* r (rand)))
        alpha (* 2 Math/PI (rand))]
    [(+ x (* radius (Math/cos alpha)))
     (+ y (* radius (Math/sin alpha)))]))

(defn jitter-x [[x y] r]
  (let [rx (- (* 2 r (rand)) r)]
    [(+ x rx) y]))

(defn jitter-y [[x y] r]
  (let [ry (- (* 2 r (rand)) r)]
    [x (+ y ry)]))
