(ns shimmers.math.geometry.polygon
  (:require
   [clojure.math :as math]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.line :as line]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
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

;; Apothem and inradius are synonyms
(defn apothem-side-length
  "Distance from center of a regular polygon with `n` sides of `side-length` to
  the midpoint of a side."
  [n side-length]
  (/ side-length (* 2 (math/tan (/ math/PI n)))))

(defn apothem-circumradius
  "Distance from center of a regular polygon with `n` sides and `circumradius` to
  the midpoint of a side."
  [n circumradius]
  (* circumradius (math/cos (/ math/PI n))))

;; https://mathworld.wolfram.com/Sagitta.html
;; sagitta = circumradius - inradius
;; h = R - r
(defn sagitta-side-length
  "Distance from a midpoint of a `side-length` face of an `n` sided regular
  polygon to the circumradius."
  [n side-length]
  (/ (* side-length (math/tan (/ math/PI (* 2 n)))) 2))

(defn sagitta-inradius
  "Distance from the midpoint of a face of an `n` sided regular polygon with
  `inradius`."
  [n inradius]
  (* inradius (math/tan (/ math/PI n)) (math/tan (/ math/PI (* 2 n)))))

(defn sagitta-circumradius
  "Distance from the midpoint of a face of an `n` sided regular polygon with
  `circumradius`."
  [n circumradius]
  (* 2 circumradius (eq/sqr (math/sin (/ math/PI (* 2 n))))))

(defn circumradius-side-length
  [n side-length]
  (/ side-length (* 2 (math/sin (/ math/PI n)))))

(defn circumradius-inradius
  [n inradius]
  (/ inradius (math/cos (/ math/PI n))))

(defn side-length-inradius
  [n inradius]
  (* 2 inradius (math/tan (/ math/PI n))))

(defn side-length-circumradius
  [n circumradius]
  (* 2 circumradius (math/sin (/ math/PI n))))

;; https://en.wikipedia.org/wiki/Regular_polygon#Circumradius
(defn regular-n-gon
  "regular polygon of `circumradius` and `n` sides.

  Polygon is rotated to ensure a flat edge is at angle 0."
  [n circumradius]
  (let [s (-> (gc/circle circumradius)
              (g/as-polygon n))]
    (if (even? n)
      (g/rotate s (/ math/PI n))
      s)))

;; TODO calculate top/bottom versions and then the other off rotations?
(defn pentagon [circumradius orientation]
  (->> (regular-n-gon 6 circumradius)
       g/vertices
       cycle
       (drop orientation)
       (take 4)
       gp/polygon2))

(comment (pentagon 1 0))

(defn sum-interior-angles-regular-polygon
  [n]
  {:pre [(> n 2)]}
  (* (- n 2) math/PI))

(defn interior-angle-regular-polygon
  [n]
  {:pre [(> n 2)]}
  (/ (sum-interior-angles-regular-polygon n) n))

(comment
  (for [n (range 3 10)]
    [n
     (sum-interior-angles-regular-polygon n)
     (interior-angle-regular-polygon n)]))

(defn dist-to-closest-point
  "Calculate distance to closest point on polygon.

  Note this is at least O(edges) of the polygon."
  [shape p]
  (-> shape
      (g/closest-point p)
      (g/dist p)))

(defn midpoint [[a b]]
  (tm/mix a b 0.5))

(defn closest-point [point points]
  (apply min-key (partial g/dist-squared point) points))

(defn connect-polygons [a b]
  (gl/line2 (closest-point (g/centroid b) (concat (map midpoint (g/edges a)) (g/vertices a)))
            (closest-point (g/centroid a) (concat (map midpoint (g/edges b)) (g/vertices b)))))
