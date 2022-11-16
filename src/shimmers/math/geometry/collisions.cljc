(ns shimmers.math.geometry.collisions
  (:require [shimmers.math.geometry.intersection :as intersect]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.utils.intersect :as isec]
            #?(:clj [thi.ng.geom.types]
               :cljs [thi.ng.geom.types :refer [Circle2 Line2 Rect2 Polygon2]])
            #?(:clj [thi.ng.geom.vector]
               :cljs [thi.ng.geom.vector :refer [Vec2]]))
  #?(:clj (:import [thi.ng.geom.types Circle2 Line2 Rect2 Polygon2]
                   [thi.ng.geom.vector Vec2])))

(defmulti overlaps?
  "Test if two shapes overlap, either at edges or if one contains the other."
  (fn [a b] [(type a) (type b)]))

;; Line2, Rect2, Polygon2, Circle2

(defmethod overlaps?
  [Line2 Polygon2] [line polygon]
  (overlaps? polygon line))

(defmethod overlaps?
  [Polygon2 Line2] [polygon {[p q] :points}]
  (or (g/contains-point? polygon p)
      (g/contains-point? polygon q)
      (when (isec/intersect-line2-edges? p q (g/edges polygon))
        true)))

(defmethod overlaps?
  [Line2 Circle2] [line circle]
  (overlaps? circle line))

(defmethod overlaps?
  [Circle2 Line2] [circle line]
  (intersect/circle-line-overlap? circle line))

(defmethod overlaps?
  [Line2 Line2] [a b]
  (contains? #{:intersect :coincident}
             (:type (g/intersect-line a b))))

(defmethod overlaps?
  [Circle2 Circle2] [a b]
  (geometry/circles-overlap? a b))

(defmethod overlaps?
  [Rect2 Rect2] [a b]
  (isec/intersect-rect-rect? a b))

(defmethod overlaps?
  [Rect2 Circle2] [r c]
  (isec/intersect-rect-circle? r c))

(defmethod overlaps?
  [Circle2 Rect2] [c r]
  (isec/intersect-rect-circle? r c))

(defmethod overlaps?
  [Polygon2 Rect2] [poly rect]
  (overlaps? poly (g/as-polygon rect)))

(defmethod overlaps?
  [Rect2 Polygon2] [rect poly]
  (overlaps? poly (g/as-polygon rect)))

(defmethod overlaps?
  [Rect2 Line2] [rect line]
  (overlaps? (g/as-polygon rect) line))

(defmethod overlaps?
  [Line2 Rect2] [line rect]
  (overlaps? (g/as-polygon rect) line))

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
  [Polygon2 Circle2] [poly circle]
  (overlaps? circle poly))

(defmethod overlaps?
  [Circle2 Polygon2] [circle poly]
  (some (fn [[p q]] (intersect/circle-segment-overlap? circle p q))
        (g/edges poly)))

;; TODO: handle cases where bounds is not a rectangle
(defmulti bounded? (fn [bounds shape] [(type bounds) (type shape)]))

(defmethod bounded?
  [Rect2 Vec2] [bounds point]
  (g/contains-point? bounds point))

(defmethod bounded?
  [Rect2 Polygon2] [bounds poly]
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
