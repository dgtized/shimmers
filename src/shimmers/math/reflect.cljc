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
(defn mod-mix [c1 c2 t]
  (let [d (Math/abs (- c2 c1))]
    (if (< d 0.5)
      (tm/mix* c1 c2 t)
      (if (< c1 c2)
        (mod (tm/mix* (+ 1 c1) c2 t) 1.0)
        (mod (tm/mix* c1 (+ 1 c2) t) 1.0)))))
