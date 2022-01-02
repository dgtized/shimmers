(ns shimmers.math.geometry
  (:require [shimmers.common.sequence :as cs]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.geometry.triangle :as triangle]
            [shimmers.math.probability :as p]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.quaternion :as quat]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Polygon2 Line2 Line3]])
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm])
  #?(:clj (:import [thi.ng.geom.types Polygon2 Line2 Line3])))

(defn generate-points
  "Generate point 2d points in space"
  ([n dist]
   (generate-points n dist dist))
  ([n dist-x dist-y] (repeatedly n #(gv/vec2 (dist-x) (dist-y)))))

;; https://stats.stackexchange.com/questions/481543/generating-random-points-uniformly-on-a-disk
(defn random-point-in-circle [_]
  (-> (gv/vec2
       (* (get _ :r) (Math/sqrt (tm/random)))
       (* tm/TWO_PI (tm/random)))
      g/as-cartesian
      (tm/+ (get _ :p))))

;; Uniformly sample points from tesselated triangles of polygon
;; https://blogs.sas.com/content/iml/2020/10/21/random-points-in-polygon.html
;; https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
(extend-type Polygon2
  g/ISample
  (random-point-inside
    [_] (->> (g/tessellate _)
             (map gt/triangle2)
             (p/weighted-by g/area)
             triangle/random-point-inside)))

;; TODO: remove once https://github.com/thi-ng/geom/pull/82 is published
(extend-type Line2
  g/IFlip
  (flip [_]
    (Line2. (vec (rseq (:points _))))))

(extend-type Line3
  g/IFlip
  (flip [_]
    (Line3. (vec (rseq (:points _))))))

(defn rotate-around-centroid [polygon t]
  (-> polygon
      g/center
      (g/rotate t)
      (g/translate (g/centroid polygon))))

(defn rotate-around
  "Rotate a polygon `theta` radians around a specific `position`."
  [polygon position theta]
  (-> polygon
      (g/translate (tm/- position))
      (g/rotate theta)
      (g/translate position)))

;; Quaternion
;; https://www.weizmann.ac.il/sci-tea/benari/sites/sci-tea.benari/files/uploads/softwareAndLearningMaterials/quaternion-tutorial-2-0-1.pdf
;; http://danceswithcode.net/engineeringnotes/quaternions/quaternions.html
;; https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
;; https://en.wikipedia.org/wiki/Gimbal_lock
;; https://developerblog.myo.com/quaternions/
(defn rotate-over-edge [poly [a b] theta]
  (let [axis (tm/- b a)
        rotation (quat/quat-from-axis-angle axis theta)]
    (-> poly
        (g/translate (tm/- a))
        (g/transform rotation)
        (g/translate a))))

(defn reflect-over-edge [c [a b]]
  (let [edge (gl/line3 a b)
        close (gl/line3 c (g/closest-point edge c))]
    (first (:points (g/reflect close edge)))))

(defn displace [polygon theta dir]
  (-> polygon
      g/center
      (g/rotate theta)
      (g/translate (tm/+ (g/centroid polygon) dir))))

(defn radial-sort
  "Counter-clockwise sort of all points around an origin point"
  [origin points]
  (sort-by (fn [p] (g/heading (tm/- p origin))) points))

(defn shape-at [shape rotation scale pos]
  (-> shape
      (g/rotate rotation)
      (g/scale-size scale)
      (g/translate pos)))

(defn confused-midpoint
  "For a given line p to q, pick a random point from the circle centered at the
  midpoint. d is a sizing factor for the radius, d of 1 yields a circle that clips
  p and q, d of 0.5 would only allow points in the middle half."
  [p q d]
  (->> (* d 0.5 (g/dist p q))
       (p/confusion-disk (tm/mix p q 0.5))
       gv/vec2))

(defn circles-overlap? [a b]
  (let [distance (+ (:r a) (:r b))]
    (< (g/dist (:p a) (:p b)) distance)))

;; TODO: extend IBoundary/contains-entity? for other shapes
(defn contains-circle? [boundary {:keys [p r]}]
  (let [[x y] p]
    (and (> (- x r) (rect/left boundary))
         (< (+ x r) (rect/right boundary))
         (< (+ y r) (rect/top boundary))
         (> (- y r) (rect/bottom boundary)))))

(defn contains-box? [boundary box]
  (and (< (rect/left boundary) (rect/left box))
       (< (rect/right box) (rect/right boundary))
       (< (rect/top box) (rect/top boundary))
       (< (rect/bottom boundary) (rect/bottom box))))

(defn point-within? [shapes point]
  (some (fn [s] (g/contains-point? s point)) shapes))

(defn split-edge [[a b] cuts]
  (->> (if (> cuts 0)
         (mapv #(tm/mix a b %) (cs/midsection (dr/var-range cuts)))
         [])
       (into [a])))

(defn shatter
  ([rect n] (shatter rect n {}))
  ([rect n {:keys [edge-splits] :or {edge-splits 3}}]
   (let [polygon (g/as-polygon rect)
         edges (g/edges polygon)]
     (->> (mapcat split-edge edges (repeatedly #(dr/random-int edge-splits)))
          gp/polygon2
          g/tessellate
          (mapv gt/triangle2)
          (triangle/decompose-into {:n n})))))
