(ns shimmers.math.geometry.points
  (:require
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.strf.core :as f]))

(defn points-delta=
  ([as bs] (points-delta= as bs tm/*eps*))
  ([as bs eps]
   (let [n (count as)]
     (and (pos? n) (= n (count bs))
          (every? true? (map (fn [a b] (tm/delta= a b eps))
                             as bs))))))

;; FIXME: use a defmulti or protocol
(defn as-str
  ([point]
   (as-str 2 point))
  ([decimals point]
   (apply f/format
          (interpose "," (repeat (count point) (f/float decimals)))
          point)))

(comment (as-str (gv/vec2 0.3 0.4))
         (as-str (gv/vec3 (/ 1 3) 0.3 0.4)))

(defn midpoint
  ([[p q]]
   (midpoint p q))
  ([p q]
   (tm/mix p q 0.5)))

(comment (midpoint (gv/vec2) (gv/vec2 1 1)))
