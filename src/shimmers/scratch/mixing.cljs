(ns shimmers.scratch.mixing
  (:require
   [clojure.math :as math]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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

;; some easing examples
(comment
  (map (fn [x] [x (math/pow (+ 1 x) 3)]) (range 0 1 0.1))
  (map (fn [x] [x (math/pow x 4)]) (range 0 1 0.1))
  (map (fn [x] [x (math/pow 4 x)]) (range 0 1 0.1))
  (map (fn [x] [x (math/pow 4 (+ 1 x))]) (range 0 1 0.1)))
