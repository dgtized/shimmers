(ns shimmers.math.geometry.group
  "Container for a set of shapes."
  (:require [thi.ng.geom.core :as geom]
            [thi.ng.geom.utils :as gu]
            [thi.ng.math.core :as tm]))

(defprotocol IGroup
  (count-children [_]))

(defrecord Group [children]
  IGroup
  (count-children [_]
    (count children))

  geom/IBounds
  (bounds [_]
    (gu/coll-bounds children))
  (width [_]
    (geom/width (geom/bounds _)))
  (height [_]
    (geom/height (geom/bounds _)))

  geom/ICenter
  ;; Not sure if strictly correct centroid, averages centroids of children
  (centroid [_]
    (tm/div (reduce tm/+ (map geom/centroid children)) (count children)))

  geom/ITranslate
  (translate [_ t]
    (Group. (mapv (fn [s] (geom/translate s t)) children))))

(defn group
  [children]
  (Group.
   (if (sequential? children)
     children
     [children])))
