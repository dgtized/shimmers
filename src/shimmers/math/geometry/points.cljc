(ns shimmers.math.geometry.points
  (:require
   [thi.ng.math.core :as tm]))

(defn points-delta=
  ([as bs] (points-delta= as bs tm/*eps*))
  ([as bs eps]
   (let [n (count as)]
     (and (pos? n) (= n (count bs))
          (every? true? (map (fn [a b] (tm/delta= a b eps))
                             as bs))))))
