(ns shimmers.math.core
  (:require
   [clojure.math :as math]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn range-subdivided [r n]
  (let [n (if (zero? n) (inc n) n)]
    (map #(* r (/ % (double n)))
         (range n))))

(comment (range-subdivided tm/TWO_PI 10)
         (range-subdivided tm/TWO_PI 2)
         (range-subdivided tm/TWO_PI 1))

(defn reflect-into
  "Map a value `v` into a space bounded by `size`, by using modular arithmetic for
  `v` with respect to `size`, and reflecting at the halfway point.

  In other words mapping into size 4 should result in values from 0..2 and 2..0."
  [v size]
  (let [reflection (/ size 2)
        v (mod v size)]
    (cond (< v reflection)
          v
          (>= v reflection)
          (- size v))))

;; TODO simplify
(defn mix-mod
  "Linear mix between `a` and `b` by `t` in a modular space `m`

  Ie `a` and `b` might be closer by moving `a` counter-clockwise towards `b`
  with 0/`m` as a point in between."
  ([a b t] (mix-mod a b t 1.0))
  ([a b t m]
   (let [d (tm/abs-diff b a)]
     (if (<= d (* 0.5 m))
       (tm/mix* a b t)
       (if (< a b)
         (mod (tm/mix* (+ m a) b t) m)
         (mod (tm/mix* a (+ m b) t) m))))))

(defn radial-mix
  "Linear mix between angles `a` and `b` by `t` in modular space around a circle."
  [a b t]
  (mix-mod a b t tm/TWO_PI))

(defn mod-between?
  "Check if `a` < `t` <= `b` in a modular space `m`."
  ([a b t] (mod-between? 1.0 a b t))
  ([m a b t]
   (let [a' (mod a m)
         b' (mod b m)
         t' (mod t m)]
     (if (< a' b')
       (< a' t' b')
       (or (< a' t') (<= t' b'))))))

(defn radians-between?
  "Check if `a` < `t` <= `b` in a modular space 2π. Used to see if angle `t` is
  between `a` and `b` as ordered."
  [a b t]
  (mod-between? tm/TWO_PI a b t))

(defn mod-distance
  "Calculate the shortest distance between `a` and `b` in modular space `m`.
  Note that distance is always positive in this space."
  ([a b] (mod-distance 1.0 a b))
  ([m a b]
   (min (mod (- a b) m)
        (mod (- b a) m))))

(comment (mod-distance 0.4 1.0)
         (mod-distance 0.6 1.0))

(defn mod-far-distance
  "Calculate the longest distance between `a` and `b` in modular space `m`.
  Note that distance is always positive in this space."
  ([a b] (mod-far-distance 1.0 a b))
  ([m a b]
   (max (mod (- a b) m)
        (mod (- b a) m))))

(comment (mod-far-distance 0.4 1.0)
         (mod-far-distance 0.6 1.0))

(defn radial-distance
  "Calculate the shortest rotational distance between `a` and `b`.
  Distances are always positive."
  [a b]
  (mod-distance tm/TWO_PI a b))

(comment (radial-distance 0 -1)
         (radial-distance -1 -5))

(defn clockwise-distance [a b]
  (- (if (< b a) (+ b tm/TWO_PI) b) a))

(defn relative-diff
  "Calculate relative difference between two positive values `a` and `b`"
  [a b]
  (/ (tm/abs-diff a b)
     (max a b)))

;; cribbed from http://clj-me.cgrand.net/2008/06/07/primes/
;; and updated to use lazy-seq
(def primes
  (lazy-cat [2]
            ((fn this [n]
               (let [potential-divisors (take-while #(<= (* % %) n) primes)]
                 (if (some #(zero? (rem n %)) (rest potential-divisors))
                   (recur (+ n 2))
                   (cons n (lazy-seq (this (+ n 2)))))))
             3)))

(defn primes-between [a b]
  (->> primes
       (drop-while #(<= % a))
       (take-while #(< % b))))

(comment
  (take 10 primes)
  (primes-between 10 50))

(defn factors [n k]
  (filter (fn [factor] (= (mod n factor) 0)) (range 2 k)))

(comment (map (fn [n] [n (factors n 9)]) (range 1 100)))

;; https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

;; https://en.wikipedia.org/wiki/Lagrange_polynomial
;; https://www.youtube.com/watch?v=4S6G-zenbFM
(defn lagrange-interpolate [points]
  (fn [x]
    (reduce (fn [sum j]
              (let [[xj yj] (nth points j)]
                (+ sum (* yj
                          (reduce (fn [prod m]
                                    (if (= m j)
                                      prod
                                      (let [xm (:x (nth points m))]
                                        (* prod (/ (- x xm) (- xj xm))))))
                                  1.0
                                  (range (count points)))))))
            0.0
            (range (count points)))))

;; https://acme.byu.edu/00000179-afb2-d74f-a3ff-bfbb158d0001/polynomialinterpolation19-pdf
(defn lagrange-barycentric [points]
  (let [ks (range (count points))
        weights (mapv (fn [j]
                        (let [xj (:x (nth points j))]
                          (reduce (fn [prod m]
                                    (if (= m j)
                                      prod
                                      (* prod (math/pow (- xj (:x (nth points m))) -1))))
                                  1.0
                                  ks)))
                      ks)]
    (fn [x]
      ;; as we divide by x - xj, replace the known points to avoid divide by zero
      (if-let [y (some (fn [[px py]] (when (tm/delta= px x) py)) points)]
        y
        (* (reduce (fn [prod m]
                     (* prod (- x (:x (nth points m)))))
                   1.0
                   ks)
           (reduce (fn [sum j]
                     (let [[xj yj] (nth points j)]
                       (+ sum (* yj (/ (nth weights j) (- x xj))))))
                   0.0
                   ks))))))

(comment
  (let [f (lagrange-interpolate [(gv/vec2 1 2) (gv/vec2 2 1) (gv/vec2 3 3)])]
    (map (fn [x] (gv/vec2 x (f x))) (range 0 4 0.25)))

  (let [f (lagrange-barycentric [(gv/vec2 1 2) (gv/vec2 2 1) (gv/vec2 3 3)])]
    (map (fn [x] (gv/vec2 x (f x))) (range 0 4 0.25))))

(defn interpolate
  "For `xs` values, smoothly interpolate using `t` in [0.0,1.0).

  The values in `xs` are distributed at even intervals along the x-axis in the
  range of `t`. The parameter `k` in [0.0,1.0), determines how long to stay at
  each values."
  [xs k t]
  (let [n (count xs)
        k (* 0.5 (mod k 1.0))
        v (* n (mod t 1.0))
        a (if (tm/delta= t 1.0)
            (dec n)
            (mod (math/floor v) n))
        b (mod (inc a) n)]
    (tm/mix* (nth xs a) (nth xs b)
             (tm/smoothstep* k (- 1.0 k) (* n (- t (/ a n)))))))

(comment
  (map (fn [t] (interpolate [0.2 0.7 0.4] 0.0 t)) (tm/norm-range 20))
  (map (fn [t] (interpolate [0.2 2.0 0.7] 0.1 t)) (tm/norm-range 20))
  (map (fn [t] (interpolate [0.2 2.0 0.7] 0.3 t)) (tm/norm-range 20))
  (map (fn [t] (interpolate [0.2 2.0 0.7] 1.0 t)) (tm/norm-range 20))
  (map (fn [t] (interpolate [0.2 2.0 0.7 3.0] 1.0 t)) (tm/norm-range 20)))
