(ns shimmers.math.geometry.group
  "Container for a set of shapes."
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.utils :as gu]
            [thi.ng.math.core :as tm]))

(defprotocol IGroup
  (count-children [_]))

(defrecord Group [children]
  IGroup
  (count-children [_]
    (count children))

  g/IBounds
  (bounds [_]
    (gu/coll-bounds children))
  (width [_]
    (g/width (g/bounds _)))
  (height [_]
    (g/height (g/bounds _)))

  g/ICenter
  ;; Not sure if strictly correct centroid, averages centroids of children
  (centroid [_]
    (tm/div (reduce tm/+ (map g/centroid children)) (count children)))

  g/ITranslate
  (translate [_ t]
    (Group. (mapv (fn [s] (g/translate s t)) children))))

(defn group
  [children]
  (Group.
   (if (sequential? children)
     children
     [children])))
