(ns shimmers.math.probability
  (:require [shimmers.common.sequence :as cs]
            [kixi.stats.distribution :as ksd]
            [thi.ng.math.core :as tm]))

(defn chance [prob]
  (< (rand) prob))

(defn happensity
  "Returns an even distribution with with probability `prob`, otherwise returns 0."
  [prob]
  (if (chance prob)
    (rand)
    0))

(defn weighted
  "Given a mapping of values to weights, choose a value biased by weight from a
  RNG source generating values from 0 to 1."
  ([weights] (weighted weights (tm/random)))
  ([weights random-value]
   (let [sample (* random-value (apply + (vals weights)))]
     (loop [cumulative 0.0
            [[choice weight] & remaining] weights]
       (when weight
         (let [sum (+ cumulative weight)]
           (if (< sample sum)
             choice
             (recur sum remaining))))))))

(defn weighted-by
  "Given a sequence of values `xs`, weight each value by a function `f` and return
  a weighted random selection."
  [f xs]
  (weighted (cs/mapping f xs)))

(comment
  (frequencies (repeatedly 1000 #(weighted {:a 0.1 :b 0.9})))
  (frequencies (repeatedly 1000 #(weighted-by inc [1 2 3]))))

(defn map-random-sample
  "Apply `xf` to the subset of `coll` selected with probability density `pf` for
  each element, with the unsampled elements intermingled as before."
  [pf xf coll]
  (map (fn [x] (if (chance (pf x)) (xf x) x)) coll))

(defn mapcat-random-sample
  "Apply `xf` to the subset of `coll` selected with probability density `pf` for
  each element, with the unsampled elements intermingled as before. `xf` must
  return a sequence."
  [pf xf coll]
  (mapcat (fn [x]
            (if (chance (pf x)) (xf x) [x]))
          coll))

(comment (map-random-sample (constantly 0.1) inc (range 10))
         (mapcat-random-sample (constantly 0.1) (fn [x] [x x]) (range 10)))

;; alternatively named cond-if?
;; however this does not address mapping n collections together?
(defn prob-if
  ([prob-fn transform-fn] (prob-if prob-fn transform-fn identity))
  ([prob-fn transform-fn identity-fn]
   (fn [x]
     (if (chance (prob-fn x))
       (transform-fn x)
       (identity-fn x)))))

(comment (map (prob-if (constantly 0.1) inc) (range 10))
         (mapcat (prob-if (constantly 0.1) (fn [x] [(inc x) x]) vector) (range 10))
         ;; This part doesn't error but is not working as expected.
         (map (prob-if (constantly 0.2) (fn [x y] [:v x y]) vector) (range 5) (reverse (range 5))))

(defn gaussian [mu sd]
  (ksd/draw (ksd/normal {:mu mu :sd sd})))

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
