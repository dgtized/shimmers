(ns shimmers.scratch.mixing
  (:require [thi.ng.math.core :as tm]
            [thi.ng.geom.vector :as gv]))

(defn inv-mix [a b v]
  (/ (- v a) (- b a)))

(defn remap [ia ib oa ob v]
  (tm/mix* oa ob (inv-mix ia ib v)))

(comment
  (for [t (tm/norm-range 5)]
    (tm/mix-with (gv/vec2 0 0) (gv/vec2 -1 1) t tm/mix*))

  (for [x (range 9 16 0.5)]
    (inv-mix 10 15 x))

  (for [x (range 2 10 0.5)]
    [x (remap 4 8 10 20 x)]))
