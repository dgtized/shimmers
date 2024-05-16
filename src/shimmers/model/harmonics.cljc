(ns shimmers.model.harmonics
  (:require
   [clojure.math :as math]
   [clojure.math.combinatorics :as mc]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn abc []
  (let [a (dr/weighted-by #(math/pow 0.9 %) (range 1 11))
        b (dr/weighted-by #(sm/lcm % a) (range 1 13))
        lcm (sm/lcm a b)
        gcd (sm/gcd a b)
        c (dr/weighted
           [[(* (min a b) (dr/random-int 1 6)) 2.0]
            [lcm (if (> lcm 1.0) 1.0 0.0)]
            [gcd (if (> gcd 1.0) 1.0 0.0)]
            [(dr/random-int 1 10) 1.0]])]
    (tm/* (gv/vec3 a b c)
          (gv/vec3 (dr/rand-nth (mc/selections [-1 1] 3))))))

(comment
  (map (fn [x] (math/pow 0.90 x)) (range 15))
  (map (fn [x] (sm/gcd 3 x))  (range 20))
  (map (fn [x] (sm/lcm 3 x))  (range 20)))
