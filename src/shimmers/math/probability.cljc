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

(defn weighted
  "Given a mapping of values to weights, randomly choose a value biased by weight"
  [weights]
  (let [sample (tm/random (apply + (vals weights)))]
    (loop [cumulative 0.0
           [[choice weight] & remaining] weights]
      (when weight
        (let [sum (+ cumulative weight)]
          (if (< sample sum)
            choice
            (recur sum remaining)))))))

(defn weighted-by
  "Given a sequence of values `xs`, weight each value by a function `f` and return
  a weighted random selection."
  [f xs]
  (weighted (cs/mapping f xs)))

(comment
  (frequencies (repeatedly 1000 #(weighted {:a 0.1 :b 0.9})))
  (frequencies (repeatedly 1000 #(weighted-by inc [1 2 3]))))

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
  (let [radius (* r (Math/sqrt (rand)))
        alpha (* 2 Math/PI (rand))]
    [(+ x (* radius (Math/cos alpha)))
     (+ y (* radius (Math/sin alpha)))]))

(defn jitter-x [[x y] r]
  (let [rx (tm/random (- r) r)]
    [(+ x rx) y]))

(defn jitter-y [[x y] r]
  (let [ry (tm/random (- r) r)]
    [x (+ y ry)]))
