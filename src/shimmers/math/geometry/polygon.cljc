(ns shimmers.math.geometry.polygon
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [Polygon2]]))
  #?(:clj (:import [thi.ng.geom.types Polygon2])))

(extend-type Polygon2
  tm/IDeltaEquals
  (delta=
    ([_ poly] (tm/delta= _ poly tm/*eps*))
    ([_ poly eps]
     (if (instance? Polygon2 poly)
       (let [vs (g/vertices _)
             other-vs (g/vertices poly)]
         (and (= (count vs) (count other-vs))
              (every? true?
                      (map (fn [a b] (tm/delta= a b eps))
                           vs
                           other-vs))))
       false))))

