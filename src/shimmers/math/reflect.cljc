(ns shimmers.math.reflect
  (:require [thi.ng.math.core :as tm]))

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
  "Linear mix between `a` and `b` by `t` in a modular space

  Ie `a` and `b` might be closer by moving `a`
  counter-clockwise towards `b`."
  ([a b t] (mix-mod a b 1.0 t))
  ([a b m t]
   (let [d (Math/abs (- b a))]
     (if (<= d (* 0.5 m))
       (tm/mix* a b t)
       (if (< a b)
         (mod (tm/mix* (+ m a) b t) m)
         (mod (tm/mix* a (+ m b) t) m))))))
