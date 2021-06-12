(ns shimmers.math.core
  (:require [thi.ng.math.core :as tm]))

(defn angles
  "Return sequence of angles from 0.0 to 2Pi subdivided by n."
  [n]
  (let [m (* 2 Math/PI)]
    (range 0 m (/ m n))))

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

(defn mod-between?
  "Check if `a` < `t` < `b` in a modular space `m`."
  ([a b t] (mod-between? 1.0 a b t))
  ([m a b t]
   (let [a' (mod a m)
         b' (mod b m)
         t' (mod t m)]
     (if (< a' b')
       (< a' t' b')
       (or (< a' t') (< t' b'))))))

(defn radians-between?
  "Check if `a` < `t` < `b` in a modular space 2π. Used to see if angle `t` is
  between `a` and `b` as ordered."
  [a b t]
  (mod-between? tm/TWO_PI a b t))

(defn mod-distance
  "Calculate the distance between `a` and `b` in modular space `m`.
  Note that distance is always positive in this space."
  ([a b] (mod-distance 1.0 a b))
  ([m a b]
   (min (mod (- a b) m)
        (mod (- b a) m))))

(comment (mod-distance 0.4 1.0)
         (mod-distance 0.6 1.0))

(defn radial-distance
  "Calculate the shortest rotational distance between `a` and `b`.
  Distances are always positive."
  [a b]
  (mod-distance tm/TWO_PI a b))

(comment (radial-distance 0 -1)
         (radial-distance -1 -5))

(defn relative-diff
  "Calculate relative difference between two positive values `a` and `b`"
  [a b]
  (/ (tm/abs-diff a b)
     (max a b)))

