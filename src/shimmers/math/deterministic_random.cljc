(ns shimmers.math.deterministic-random
  "Provides a shared, seeded random number generator for deterministic procedural
  generation."
  (:refer-clojure :exclude [rand-nth shuffle random-sample])
  (:require
   [clojure.math :as math]
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
  (rand-int (math/pow 2 32)))

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
  ([n] (int (math/floor (random n))))
  ([a b] (int (math/floor (random a b)))))

(defn random-tau []
  (random eq/TAU))

(defn seed []
  (random-int (math/pow 2 32)))

;; https://ivandianov.com/circular-random/
(defn circular-random []
  (- 1 (math/sqrt (- 1 (eq/sqr (random))))))

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

(defn happensity [prob]
  (if (< (random-double) prob) (random-double) 0))

(defn random-sign
  "Generate a sign, ie 1 or -1 with probability `p`."
  ([] (random-sign 0.5))
  ([p] (if (chance p) 1 -1)))

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
  ([f xs]
   (weighted-by f (random-double) xs))
  ([f rv xs]
   (weighted (mapv (fn [v] [v (f v)]) xs)
             rv)))

(comment
  (frequencies (repeatedly 1000 #(weighted {:a 0.1 :b 0.9} (random-double))))
  (frequencies (repeatedly 1000 #(weighted-by inc [1 2 3]))))

(defn random-sample
  "Returns items from coll with probability of prob (0.0 - 1.0)."
  [prob coll]
  (if (ifn? prob)
    (filter (fn [x] (chance (prob x))) coll)
    (filter (partial chance prob) coll)))

(comment
  (random-sample 0.2 (range 20))
  (random-sample (constantly 0.2) (range 20)))

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

(comment (map-random-sample (constantly 0.1) (partial * 2) (range 10))
         (mapcat-random-sample (constantly 0.1) (fn [x] [x x]) (range 10)))

(defn jitter
  "Create a random unit vector and then scale it by `amount` to use as noise."
  [amount]
  (v/polar amount (random-tau)))

(defn jitter-x [[x y] r]
  (let [rx (random (- r) r)]
    (gv/vec2 (+ x rx) y)))

(defn jitter-y [[x y] r]
  (let [ry (random (- r) r)]
    (gv/vec2 x (+ y ry))))

(defn random-vertex []
  (gv/vec2 (random-double) (random-double)))

(defn randvec2
  ([] (tm/normalize (gv/vec2 (random -1 1) (random -1 1))))
  ([n] (tm/normalize (gv/vec2 (random -1 1) (random -1 1)) n)))

(def ^:constant MAX-INT (math/pow 2 31))
(defn gaussian
  ([] (gaussian 0 1))
  ([mu sd]
   (ksd/draw (ksd/normal {:mu mu :sd (+ tm/*eps* (abs sd))})
             {:seed (random-int MAX-INT)})))

(defn sample-between
  "Returns a function to sample an rng, but only accept values within the clamping range.

  This useless for statistics, but helpful for pretty graphics."
  [rng min max]
  (fn []
    (->> rng
         repeatedly
         (drop-while (fn [x] (or (<= x min) (>= x max))))
         first)))

(comment
  (repeatedly 100 (sample-between #(gaussian 0.5 0.5) 0.0 1.0)))

(defn pareto [scale shape]
  (/ scale (math/pow (random) (/ 1 shape))))

(comment
  (repeatedly 20 #(pareto 0.125 1.16)))

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
  "Generate an ordered range of values from [`lower`..`upper`] with normal spacing
  `mu`, and standard-deviation `sd`. `include-edges?` determines if the `lower`
  and `upper` values should be included in the output sequence."
  ([mu sd]
   (gaussian-range mu sd false [0.0 1.0]))
  ([mu sd include-edges?]
   (gaussian-range mu sd include-edges? [0.0 1.0]))
  ([mu sd include-edges? [lower upper]]
   (let [range (->> #(abs (gaussian mu sd))
                    repeatedly
                    (reductions + lower)
                    (take-while #(< % upper))
                    (drop 1)
                    doall)]
     (if include-edges?
       (concat [lower] range [upper])
       range))))

(comment (gaussian-range 0.1 0.05)
         (gaussian-range 0.2 0.1 true)
         (gaussian-range 0.2 0.1 true [-0.05 1.05]))

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

(defn noise-seed
  ([] (noise-seed 100))
  ([n] (gv/vec2 (random n) (random n))))

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
     :std-dev (math/sqrt variance)}))

(comment
  (summary-stats (range 0 1 0.001))
  (summary-stats (repeatedly 10000 #(gaussian 2 0.2)))
  [(summary-stats (repeatedly 10000 #(gaussian 0 0.5)))
   (summary-stats (repeatedly 10000 #(abs (gaussian 0 0.5))))]
  (summary-stats (repeatedly 10000 #(noise/noise2 (random 1000) (random 1000))))
  (summary-stats (repeatedly 10000 #(noise-at-point (gv/vec2) 0.5 (noise-seed)))))

(comment
  ;; pareto is λ X^(-1/κ) where X is a uniform rng
  (ksd/sample 100 (ksd/pareto {:scale 0.1 :shape 0.9}))
  ;; probability values < 2?
  (ksd/cdf (ksd/pareto {:scale 1 :shape 5}) 2)
  ;; value at 90% percent?
  (ksd/quantile (ksd/pareto {:scale 1 :shape 7.5}) 0.9)
  (ksd/quantile (ksd/pareto {:scale 1 :shape 1}) 0.9)
  (summary-stats (mapv (fn [x] (let [p (* 1.0 (math/pow x (/ -1 8)))]
                                p))
                       (rest (range 0 1 0.01))))
  (summary-stats (map #(min % 2) (ksd/sample 1000 (ksd/pareto {:scale 0.75 :shape 8}))))
  (summary-stats (ksd/sample 1000 (ksd/log-normal {:mu 0.0 :sd 0.2})))
  (summary-stats (ksd/sample 1000 (ksd/normal {:mu 0.0 :sd 0.2}))))

(defn ceiled-sample
  "Sample `f` for values less than `ceiling`."
  [ceiling f]
  (->> f
       repeatedly
       (drop-while (fn [x] (> x ceiling)))
       first))

(defn floored-sample
  "Sample `f` for values greater than `floor`."
  [floor f]
  (->> f
       repeatedly
       (drop-while (fn [x] (< x floor)))
       first))

(defn clamped-sample
  "Sample `f` for values between `floor` and `ceiling` inclusive."
  [floor ceiling f]
  (->> f
       repeatedly
       (drop-while (fn [x] (or (< x floor)
                              (> x ceiling))))
       first))

(comment (summary-stats (repeatedly 1000 (fn [] (ceiled-sample 10 (fn [] (pareto 1 2))))))
         (summary-stats (repeatedly 1000 (fn [] (floored-sample 2 (fn [] (pareto 1 2))))))
         (summary-stats (repeatedly 1000 (fn [] (clamped-sample 2 8 (fn [] (pareto 1 2)))))))
