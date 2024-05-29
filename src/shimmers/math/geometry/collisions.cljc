(ns shimmers.math.geometry.collisions
  (:require [clojure.math.combinatorics :as mc]
            [shimmers.math.geometry.intersection :as intersect]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.utils.intersect :as isec]
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]
            #?(:clj [thi.ng.geom.types]
               :cljs [thi.ng.geom.types :refer [Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]])
            #?(:clj [thi.ng.geom.vector]
               :cljs [thi.ng.geom.vector :refer [Vec2]]))
  #?(:clj (:import [thi.ng.geom.types Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]
                   [thi.ng.geom.vector Vec2])))

(defmulti overlaps?
  "Test if two shapes overlap, either at edges or if one contains the other."
  (fn [a b] [(type a) (type b)]))

;; Circle2, Line2, Polygon2, Rect2, Triangle2
;; TODO: Vec2?

;; Circle2
(defmethod overlaps?
  [Circle2 Circle2] [a b]
  (geometry/circles-overlap? a b))

(defmethod overlaps?
  [Circle2 Line2] [circle line]
  (intersect/circle-line-overlap? circle line))

(defmethod overlaps?
  [Circle2 Polygon2] [circle poly]
  (or (g/contains-point? poly (:p circle))
      (some (fn [[p q]] (intersect/circle-segment-overlap? circle p q))
            (g/edges poly))))

(defmethod overlaps?
  [Circle2 Rect2] [c r]
  (isec/intersect-rect-circle? r c))

(defmethod overlaps?
  [Circle2 Triangle2] [circle triangle]
  (overlaps? (g/as-polygon triangle) circle))

;; Line2
(defmethod overlaps?
  [Line2 Circle2] [line circle]
  (overlaps? circle line))

(defmethod overlaps?
  [Line2 Line2] [a b]
  (contains? #{:intersect :coincident}
             (:type (g/intersect-line a b))))

(defmethod overlaps?
  [Line2 LineStrip2] [{[p q] :points} b]
  (when (isec/intersect-line2-edges? p q (g/edges b))
    true))

(defmethod overlaps?
  [Line2 Polygon2] [line polygon]
  (overlaps? polygon line))

(defmethod overlaps?
  [Line2 Rect2] [line rect]
  (overlaps? (g/as-polygon rect) line))

(defmethod overlaps?
  [Line2 Triangle2] [line triangle]
  (overlaps? (g/as-polygon triangle) line))

;; Linestrip2
(defmethod overlaps?
  [LineStrip2 Line2] [a {[p q] :points}]
  (when (isec/intersect-line2-edges? p q (g/edges a))
    true))

(defmethod overlaps?
  [LineStrip2 LineStrip2] [a b]
  (some (fn [[p q]]
          (when (isec/intersect-line2-edges? p q (g/edges b))
            true))
        (g/edges a)))

;; Polygon2
(defmethod overlaps?
  [Polygon2 Circle2] [poly circle]
  (overlaps? circle poly))

(defmethod overlaps?
  [Polygon2 Line2] [polygon {[p q] :points}]
  (or (g/contains-point? polygon p)
      (g/contains-point? polygon q)
      (when (isec/intersect-line2-edges? p q (g/edges polygon))
        true)))

;; consider a triangle overlapping a square, with one point of triangle in
;; square, but no points of square in triangle
(defmethod overlaps?
  [Polygon2 Polygon2] [a b]
  (when (or (some (fn [point] (g/contains-point? b point)) (g/vertices a))
            (some (fn [point] (g/contains-point? a point)) (g/vertices b))
            (some (fn [[p q]] (isec/intersect-line2-edges? p q (g/edges b)))
                  (g/edges a)))
    true))

(defmethod overlaps?
  [Polygon2 Rect2] [poly rect]
  (overlaps? poly (g/as-polygon rect)))

(defmethod overlaps?
  [Polygon2 Triangle2] [polygon triangle]
  (overlaps? polygon (g/as-polygon triangle)))

;; Triangle2
(defmethod overlaps?
  [Triangle2 Circle2] [triangle circle]
  (overlaps? (g/as-polygon triangle) circle))

(defmethod overlaps?
  [Triangle2 Line2] [triangle line]
  (overlaps? (g/as-polygon triangle) line))

(defmethod overlaps?
  [Triangle2 Polygon2] [triangle polygon]
  (overlaps? polygon (g/as-polygon triangle)))

(defmethod overlaps?
  [Triangle2 Rect2] [triangle rect]
  (overlaps? (g/as-polygon rect) (g/as-polygon triangle)))

(defmethod overlaps?
  [Triangle2 Triangle2] [a b]
  (overlaps? (g/as-polygon a) (g/as-polygon b)))

;; Rectangle
(defmethod overlaps?
  [Rect2 Circle2] [r c]
  (isec/intersect-rect-circle? r c))

(defmethod overlaps?
  [Rect2 Line2] [rect line]
  (overlaps? (g/as-polygon rect) line))

(defmethod overlaps?
  [Rect2 Polygon2] [rect poly]
  (overlaps? poly (g/as-polygon rect)))

(defmethod overlaps?
  [Rect2 Rect2] [a b]
  (isec/intersect-rect-rect? a b))

(defmethod overlaps?
  [Rect2 Triangle2] [rect triangle]
  (overlaps? (g/as-polygon rect) (g/as-polygon triangle)))

;; TODO: handle cases where bounds is not a rectangle, circle / convex
;; TODO: Add Triangle2 support since it's convex
;; TODO: support rect2/rect2, circle2/circle2, etc
(defmulti bounded?
  "A `shape` or point is completely contained within `bounds`."
  (fn [bounds shape] [(type bounds) (type shape)]))

(defmethod bounded?
  [Triangle2 Vec2] [bounds point]
  (g/contains-point? bounds point))

(defmethod bounded?
  [Triangle2 Line2] [bounds {[p q] :points}]
  (and (g/contains-point? bounds p)
       (g/contains-point? bounds q)))

(defmethod bounded?
  [Triangle2 Triangle2] [bounds triangle]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices triangle)))

(defmethod bounded?
  [Triangle2 Rect2] [bounds rectangle]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices rectangle)))

(defmethod bounded?
  [Triangle2 Polygon2] [bounds polygon]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices polygon)))

;; FIXME: this works for a subset of circles in triangles but will exclude some
;; bounded circles
(defmethod bounded?
  [Triangle2 Circle2] [bounds circle]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices (g/bounds circle))))

(defmethod bounded?
  [Rect2 Vec2] [bounds point]
  (g/contains-point? bounds point))

(defmethod bounded?
  [Rect2 Polygon2] [bounds poly]
  (every? (fn [p] (g/contains-point? bounds p)) (g/vertices poly)))

(defmethod bounded?
  [Rect2 Triangle2] [bounds poly]
  (every? (fn [p] (g/contains-point? bounds p)) (g/vertices poly)))

(defmethod bounded?
  [Rect2 Circle2] [bounds circle]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices (g/bounds circle))))

(defmethod bounded?
  [Rect2 Line2] [bounds line]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices line)))

(defmethod bounded?
  [Rect2 Rect2] [{[x y] :p [w h] :size} {[rx ry] :p [rw rh] :size}]
  (and (>= rx x) (>= ry y)
       (<= (+ rx rw) (+ x w))
       (<= (+ ry rh) (+ y h))))

(defmethod bounded?
  [Circle2 Vec2] [bounds point]
  (g/contains-point? bounds point))

(defmethod bounded?
  [Circle2 Polygon2] [bounds polygon]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices polygon)))

(defmethod bounded?
  [Circle2 Line2] [bounds {[p q] :points}]
  (and (g/contains-point? bounds p)
       (g/contains-point? bounds q)))

(defmethod bounded?
  [Circle2 Rect2] [bounds rect]
  (every? (fn [p] (g/contains-point? bounds p))
          (g/vertices rect)))

(defmethod bounded?
  [Circle2 Circle2] [{c :p radius :r} {:keys [p r]}]
  (<= (g/dist c p) (- radius r)))

(defmethod bounded?
  [Polygon2 Vec2] [bounds point]
  (g/contains-point? bounds point))

;; FIXME: optimize better for concave shapes?
(defmethod bounded?
  [Polygon2 Triangle2] [bounds triangle]
  (and (every? (fn [p] (g/contains-point? bounds p))
               (g/vertices triangle))
       (not-any? (fn [p] (g/contains-point? triangle p))
                 (g/vertices bounds))))

(defmethod bounded?
  [Polygon2 Rect2] [bounds rect]
  (and (every? (fn [p] (g/contains-point? bounds p))
               (g/vertices rect))
       (not-any? (fn [p] (g/contains-point? rect p))
                 (g/vertices bounds))))

(defmethod bounded?
  [Polygon2 Circle2] [bounds circle]
  (and (every? (fn [p] (g/contains-point? bounds p))
               (g/vertices (g/bounds circle)))
       (not-any? (fn [p] (g/contains-point? circle p))
                 (g/vertices bounds))))

(defmethod bounded?
  [Polygon2 Polygon2] [bounds poly]
  (and (every? (fn [p] (g/contains-point? bounds p))
               (g/vertices poly))
       (not-any? (fn [p] (g/contains-point? poly p))
                 (g/vertices bounds))))

(defmulti coincident-edge?
  "Test if shapes `a` and `b` have an edge that touches for some distance.

  Note this may still include intersecting shapes if say `a` is inside `b` but
  touches on one edge."
  (fn [a b] [(type a) (type b)]))

(defmethod coincident-edge?
  [Rect2 Rect2] [a b]
  (let [[ax1 ay1] (:p a)
        [ax2 ay2] (rect/top-right a)
        [bx1 by1] (:p b)
        [bx2 by2] (rect/top-right b)]
    (cond (or (> ax1 bx2) (> bx1 ax2))
          false
          (or (> ay1 by2) (> by1 ay2))
          false
          :else (or (tm/delta= ax1 bx2)
                    (tm/delta= ax2 bx1)
                    (tm/delta= ay1 by2)
                    (tm/delta= ay2 by1)))))

(defn coincident-segment? [[p q] [r s]]
  (let [{:keys [type] :as hit} (isec/intersect-line2-line2? p q r s)]
    (when (= type :coincident)
      [(:p hit) (:q hit)])))

(defn coincident-polygon? [a b]
  (some (fn [edge-a]
          (some (fn [edge-b] (coincident-segment? edge-a edge-b))
                (g/edges b)))
        (g/edges a)))

(defmethod coincident-edge?
  [Polygon2 Polygon2] [a b]
  (some? (coincident-polygon? a b)))

(defmethod coincident-edge?
  [Rect2 Polygon2] [a b]
  (some? (coincident-polygon? (g/as-polygon a) b)))

(defmethod coincident-edge?
  [Polygon2 Rect2] [a b]
  (some? (coincident-polygon? a (g/as-polygon b))))

(defn intersecting-points
  "Finds all intersection points along a line through a set of edges sorted by
  distance from `rp`."
  [rp rq edges]
  (->> edges
       (sequence
        (comp
         (map (fn [[p q]] (isec/intersect-line2-line2? rp rq p q)))
         (filter (fn [isec]
                   (when (get isec :p) (= :intersect (get isec :type)))))
         (map (fn [isec] (let [p (get isec :p)
                              d (g/dist-squared rp p)]
                          [p d])))))
       (sort-by second)
       (map first)))

;; TODO: should there be a helper for finding all intersecting points between two shapes?
;; see also intertwined/intersections

;; FIXME shoudl this be iff, ie only the case if coincident-point is unique?
(defmulti coincident-point?
  "Test if shapes `a` and `b` share a point in common, either at a vertice or along
  an intersecting edge. Returns contact point if touching"
  (fn [a b] [(type a) (type b)]))

;; FIXME: some means this likely does not work if there are multiple intersections
(defmethod coincident-point?
  [Polygon2 Polygon2] [a b]
  (or (some (fn [[ap bp]] (when (tm/delta= ap bp) ap))
            (mc/cartesian-product (g/vertices a) (g/vertices b)))
      (some (fn [[a-edge b-edge]]
              ;; some intersection
              (when-let [isec (intersect/segment-intersect a-edge b-edge)]
                ;; where the vertices are the same as the intersection point
                (some (fn [p] (when (tm/delta= isec p) isec))
                      (concat a-edge b-edge))))
            (mc/cartesian-product (g/edges a) (g/edges b)))))

(defmulti adjacent?
  "Test if shapes `a` and `b` share a vertice or an edge, but do not intersect inside."
  (fn [a b] [(type a) (type b)]))
