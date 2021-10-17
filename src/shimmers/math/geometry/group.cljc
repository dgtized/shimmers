(ns shimmers.math.geometry.group
  "Container for a set of shapes."
  (:require [thi.ng.geom.core :as geom]
            [thi.ng.geom.utils :as gu]))

(defrecord Group [children]
  geom/IBounds
  (bounds [_]
    (gu/coll-bounds children))

  geom/ITranslate
  (translate [_ t]
    (Group. (mapv (fn [s] (geom/translate s t)) children))))

(defn group
  [children]
  (Group.
   (if (sequential? children)
     children
     [children])))
