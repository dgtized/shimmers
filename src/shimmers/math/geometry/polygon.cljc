(ns shimmers.math.geometry.polygon
  (:require
   [shimmers.math.equations :as eq]
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

;; Apothem and inradius are synonyms
(defn apothem-side-length
  "Distance from center of a regular polygon with `n` sides of `side-length` to
  the midpoint of a side."
  [n side-length]
  (/ side-length (* 2 (Math/tan (/ Math/PI n)))))

(defn apothem-circumradius
  "Distance from center of a regular polygon with `n` sides and `circumradius` to
  the midpoint of a side."
  [n circumradius]
  (* circumradius (Math/cos (/ Math/PI n))))

;; https://mathworld.wolfram.com/Sagitta.html
;; sagitta = circumradius - inradius
;; h = R - r
(defn sagitta-side-length
  "Distance from a midpoint of a `side-length` face of an `n` sided regular
  polygon to the circumradius."
  [n side-length]
  (/ (* side-length (Math/tan (/ Math/PI (* 2 n)))) 2))

(defn sagitta-inradius
  "Distance from the midpoint of a face of an `n` sided regular polygon with
  `inradius`."
  [n inradius]
  (* inradius (Math/tan (/ Math/PI n)) (Math/tan (/ Math/PI (* 2 n)))))

(defn sagitta-circumradius
  "Distance from the midpoint of a face of an `n` sided regular polygon with
  `circumradius`."
  [n circumradius]
  (* 2 circumradius (eq/sqr (Math/sin (/ Math/PI (* 2 n))))))

