(ns shimmers.math.deterministic-random
  "Provides a shared, seeded random number generator for deterministic procedural
  generation."
  (:refer-clojure :exclude [rand-nth shuffle random-sample])
  (:require
   [clojure.test.check.random :as tcr]
   [kixi.stats.distribution :as ksd]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.math.noise :as noise]))

(defonce shared-rng (atom (tcr/make-random)))

(defn random-seed [n]
  (reset! shared-rng (tcr/make-random n)))

(defn fresh-seed-value []
  (rand-int (Math/pow 2 32)))

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

(defn shuffle [coll]
  (->> coll
       (map (fn [x] [x (random-double)]))
       (sort-by second)
       (map first)))

;; TODO: some sort of protocol to swap in seeded random?
;; Or optimize such that cost is negligable?
(defn chance [prob]
  (< (random-double) prob))

(defn weighted
  "Given a pairing of values to weights, randomly choose a value biased by weight.

  With two arguments, use `v` to specify a specific element as a percent of the
  total weight.

  Warning: if weights are a map and keys are anonymous functions this may not be
  deterministic as the sort order of the map may change ordering of weights. In
  that case use an ordered sequence of (choice, weight) pairs."
  ([weights] (weighted weights (random-double)))
  ([weights v]
   (let [sample (* v (apply + (map second weights)))]
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
  (weighted (mapv (fn [v] [v (f v)]) xs)))

(defn random-sample
  "Returns items from coll with probability of prob (0.0 - 1.0)."
  [prob coll]
  (filter (fn [_] (< (random-double) prob)) coll))

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

(defn jitter
  "Create a random unit vector and then scale it by `amount` to use as noise."
  [amount]
  (v/polar amount (random tm/TWO_PI)))

(defn jitter-x [[x y] r]
  (let [rx (random (- r) r)]
    [(+ x rx) y]))

(defn jitter-y [[x y] r]
  (let [ry (random (- r) r)]
    [x (+ y ry)]))

(defn random-vertex []
  (gv/vec2 (random-double) (random-double)))

(defn randvec2
  ([] (tm/normalize (gv/vec2 (random -1 1) (random -1 1))))
  ([n] (tm/normalize (gv/vec2 (random -1 1) (random -1 1)) n)))

(def ^:constant MAX-INT (Math/pow 2 32))
(defn gaussian
  ([] (gaussian 0 1))
  ([mu sd]
   (ksd/draw (ksd/normal {:mu mu :sd (+ tm/*eps* (Math/abs sd))})
             {:seed (random-int MAX-INT)})))

(defn var-range
  [n]
  {:pre [(pos-int? n)]}
  (let [dt (/ 1.0 (inc n))]
    (sort (concat [0.0]
                  (->> #(gaussian dt (* dt 0.2))
                       (repeatedly n)
                       (reductions +))
                  [1.0]))))

(comment (var-range 1)
         (var-range 2)
         (var-range 5))

;; tm/norm-range but with random offsets and an uncertain number of samples
(defn density-range
  "Generate an ordered range of values from [0..1] with minimum spacing
  `min-offset`, and maximum spacing `max-offset`."
  ([min-offset max-offset]
   (->> #(random min-offset max-offset)
        repeatedly
        (reductions +)
        (take-while #(< % 1.0))
        doall))
  ([min-offset max-offset include-edges?]
   (let [range (density-range min-offset max-offset)]
     (if include-edges?
       (concat [0.0] range [1.0])
       range))))

(comment (count (density-range 0.1 0.2))
         (count (density-range 0.1 0.2 true)))

(defn gaussian-range
  "Generate an ordered range of values from [0..1] with normal spacing `mu`, and
  standard-deviation `sd`."
  ([mu sd]
   (->> #(abs (gaussian mu sd))
        repeatedly
        (reductions +)
        (take-while #(< % 1.0))
        doall))
  ([mu sd include-edges?]
   (let [range (gaussian-range mu sd)]
     (if include-edges?
       (concat [0.0] range [1.0])
       range))))

(comment (gaussian-range 0.1 0.05))

;; (defn random-swap [xs]
;;   (let [n (count xs)
;;         i (random-int (dec n))]
;;     (concat (take (dec i) xs)
;;             [(nth xs (inc i))
;;              (nth xs i)]
;;             (drop (inc i) xs))))
;; (comment (random-swap (range 5)))

(defn cyclic
  "Given a collection of values, return a function which will generate the values
  in the collection on each call."
  [coll]
  (let [len (count coll)
        iter (atom 0)]
    (fn []
      (let [i @iter]
        (swap! iter inc)
        (nth coll (mod i len))))))

(defn noise-at-point [seed scale p]
  (let [[x y] (tm/+ seed (tm/* p scale))]
    (* 0.5 (+ 1.0 (noise/noise2 x y)))))

(defn noise-at-point-01 [seed scale p]
  (let [[x y] (tm/+ seed (tm/* p scale))]
    (+ 0.5 (noise/noise2 x y))))

(defn summary-stats [xs]
  (let [n (count xs)
        avg (/ (reduce + xs) n)
        variance (/ (reduce (fn [acc x] (+ acc (eq/sqr (- x avg)))) xs) n)
        sorted (sort xs)
        midpoint (/ n 2)]
    {:n-samples n
     :min (apply min xs)
     :max (apply max xs)
     :quartiles (mapv (fn [q] (nth sorted (int (* (inc n) (/ q 4))))) [1 2 3])
     :median (if (even? n)
               (/ (+ (nth sorted midpoint) (nth sorted (inc midpoint))) 2)
               (nth sorted (int midpoint)))
     :average avg
     :variance variance
     :std-dev (Math/sqrt variance)}))

(comment
  (summary-stats (range 0 1 0.001))
  (summary-stats (repeatedly 10000 #(gaussian 2 0.2)))
  (summary-stats (repeatedly 10000 #(noise/noise2 (random 1000) (random 1000))))
  (summary-stats (repeatedly 10000 #(noise-at-point (gv/vec2) 0.5 (gv/vec2 (random 1000) (random 1000))))))

(comment
  ;; pareto is λ X^(-1/κ) where X is a uniform rng
  (ksd/sample 100 (ksd/pareto {:scale 0.1 :shape 0.9}))
  ;; probability values < 2?
  (ksd/cdf (ksd/pareto {:scale 1 :shape 5}) 2)
  ;; value at 90% percent?
  (ksd/quantile (ksd/pareto {:scale 1 :shape 7.5}) 0.9)
  (ksd/quantile (ksd/pareto {:scale 1 :shape 1}) 0.9)
  (summary-stats (mapv (fn [x] (let [p (* 1.0 (Math/pow x (/ -1 8)))]
                                p))
                       (rest (range 0 1 0.01))))
  (summary-stats (map #(min % 2) (ksd/sample 1000 (ksd/pareto {:scale 0.75 :shape 8}))))
  (summary-stats (ksd/sample 1000 (ksd/log-normal {:mu 0.0 :sd 0.2})))
  (summary-stats (ksd/sample 1000 (ksd/normal {:mu 0.0 :sd 0.2}))))
