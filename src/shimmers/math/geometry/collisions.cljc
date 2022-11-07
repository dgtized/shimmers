(ns shimmers.math.geometry.collisions
  (:require [shimmers.math.geometry.intersection :as intersect]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.utils.intersect :as isec]
            #?(:clj [thi.ng.geom.types]
               :cljs [thi.ng.geom.types :refer [Circle2 Line2 Polygon2]]))
  #?(:clj (:import [thi.ng.geom.types Circle2 Line2 Polygon2])))

(defmulti overlaps? (fn [a b] [(type a) (type b)]))

(defmethod overlaps?
  [Line2 Polygon2]
  [{[p q] :points} polygon]
  (or (g/contains-point? polygon p)
      (g/contains-point? polygon q)
      (when (isec/intersect-line2-edges? p q (g/edges polygon))
        true)))

(defmethod overlaps?
  [Polygon2 Line2]
  [polygon {[p q] :points}]
  (or (g/contains-point? polygon p)
      (g/contains-point? polygon q)
      (when (isec/intersect-line2-edges? p q (g/edges polygon))
        true)))

(defmethod overlaps?
  [Line2 Circle2]
  [line circle]
  (when (intersect/circle-segment-intersection circle line)
    true))

(defmethod overlaps? [Circle2 Line2] [circle line]
  (when (intersect/circle-segment-intersection circle line)
    true))

(defmethod overlaps?
  [Line2 Line2]
  [a b]
  (contains? #{:intesect :coincident}
             (:type (g/intersect-line a b))))

(defmethod overlaps?
  [Circle2 Circle2]
  [a b]
  (geometry/circles-overlap? a b))

(defmethod overlaps?
  [Polygon2 Polygon2]
  [a b]
  (when (some (fn [point] (g/contains-point? b point)) (g/vertices a))
    true))

(defmethod overlaps?
  [Polygon2 Circle2]
  [poly circle]
  (when (some (fn [point] (g/contains-point? circle point)) (g/vertices poly))
    true))

(defmethod overlaps?
  [Circle2 Polygon2]
  [circle poly]
  (when (some (fn [point] (g/contains-point? circle point)) (g/vertices poly))
    true))
