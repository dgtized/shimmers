(ns shimmers.math.geometry.polygon
  (:require
   [thi.ng.geom.circle :as gc]
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

;; https://en.wikipedia.org/wiki/Regular_polygon#Circumradius
(defn regular-n-gon
  "regular polygon with `n` sides, where each face is `side-length`

  Polygon is rotated to ensure a flat edge is at angle 0."
  [n side-length]
  (let [s (-> (gc/circle (/ side-length (* 2 (Math/sin (/ Math/PI n)))))
              (g/as-polygon n))]
    (if (even? n)
      (g/rotate s (/ Math/PI n))
      s)))

