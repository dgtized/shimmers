(ns shimmers.math.reflect)

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
