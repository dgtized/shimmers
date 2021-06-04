(ns shimmers.math.deterministic-random
  "Provides a shared, seeded random number generator for deterministic procedural
  generation."
  (:refer-clojure :exclude [rand-nth])
  (:require [clojure.test.check.random :as tcr]))

(defonce shared-rng (atom (tcr/make-random)))

(defn random-seed [n]
  (reset! shared-rng (tcr/make-random n)))

(defn random-double []
  (let [[r1 r2] (tcr/split @shared-rng)]
    (reset! shared-rng r2)
    (tcr/rand-double r1)))

(defn random
  "Deterministic random with signature of `thi.ng.math/random`."
  ([] (random-double))
  ([n] (* n (random-double)))
  ([a b] (+ (* (- b a) (random-double)) a)))

(defn random-int
  ([n] (Math/floor (random n)))
  ([a b] (Math/floor (random a b))))

(defn rand-nth [coll]
  (nth coll (random-int (count coll))))

(comment (do (random-seed 1000)
             (repeatedly 10 #(random-int 8)))
         (do (random-seed 10)
             (repeatedly 10 #(rand-nth (range 8))))

         (do (random-seed 6)
             (repeatedly 6 #(random-double))))

;; TODO: some sort of protocol to swap in seeded random?
;; Or optimize such that cost is negligable?
(defn chance [prob]
  (< (random-double) prob))

(defn weighted
  "Given a mapping of values to weights, randomly choose a value biased by weight"
  [weights]
  (let [sample (random (apply + (vals weights)))]
    (loop [cumulative 0.0
           [[choice weight] & remaining] weights]
      (when weight
        (let [sum (+ cumulative weight)]
          (if (< sample sum)
            choice
            (recur sum remaining)))))))

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
