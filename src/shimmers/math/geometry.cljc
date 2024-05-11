(ns shimmers.math.geometry
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.sequence :as cs]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.quaternion :as quat]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Polygon2]])
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm])
  #?(:clj (:import [thi.ng.geom.types Polygon2])))

;; Uniformly sample points from tesselated triangles of polygon
;; https://blogs.sas.com/content/iml/2020/10/21/random-points-in-polygon.html
;; https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
(extend-type Polygon2
  g/ISample
  (random-point-inside
    [_] (->> (g/tessellate _)
             (map gt/triangle2)
             (dr/weighted-by g/area)
             triangle/random-point-inside)))

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
  [rect {:keys [edge-splits] :as opts :or {edge-splits 3}}]
  {:pre [(pos? (get opts :n -1))]}
  (let [polygon (g/as-polygon rect)
        edges (g/edges polygon)]
    (->> (mapcat split-edge edges (repeatedly #(dr/random-int edge-splits)))
         gp/polygon2
         g/tessellate
         (mapv gt/triangle2)
         (triangle/decompose-into (dissoc opts :edge-splits)))))

(defn inset-rectangle [rect amount]
  (-> rect
      g/as-polygon
      (poly-detect/inset-polygon amount)
      g/bounds))

(defn manhattan-to-rectangle
  "Manhattan distance to a `rect` edge from a `point`.

  Returns [0,0] for any internal points."
  [rect point]
  (tm/max (tm/- point (rect/top-right rect))
          (tm/- (rect/bottom-left rect) point)
          (gv/vec2)))

(comment
  (manhattan-to-rectangle (rect/rect 3 1 3 2) (gv/vec2))
  (manhattan-to-rectangle (rect/rect 1 3 3 2) (gv/vec2 1.5 3.5))
  (manhattan-to-rectangle (rect/rect 1 3 3 2) (gv/vec2 4 6))
  (manhattan-to-rectangle (rect/rect 1 3 3 2) (gv/vec2 5 5))
  (manhattan-to-rectangle (rect/rect -3 -3 2 1) (gv/vec2)))

;; cribbed from https://gist.github.com/jphastings/316058
(defn percent-circle-overlap [{pa :p ra :r} {pb :p rb :r}]
  (let [d (g/dist pa pb)
        r-small (min ra rb)
        r-big (max ra rb)]
    (cond (>= d (+ ra rb))
          0.0
          (>= r-big (+ d r-small))
          1.0
          :else
          (let [x1 (/ (+ (eq/sqr d)
                         (- (eq/sqr r-small))
                         (eq/sqr r-big))
                      (* 2 d))
                x2 (abs (- d x1))
                y (math/sqrt (- (eq/sqr r-big) (eq/sqr x1)))
                a-big (- (* (eq/sqr r-big) (math/acos (/ x1 r-big)))
                         (* x1 y))
                a-small (- (* (eq/sqr r-small) (math/acos (/ x2 r-small)))
                           (* x2 y))
                a-small (if (> x1 d) (- (* math/PI (eq/sqr r-small)) a-small) a-small)
                overlap-area (+ a-small a-big)
                total-area (- (* math/PI (+ (eq/sqr r-big) (eq/sqr r-small))) overlap-area)]
            (/ overlap-area total-area)))))
