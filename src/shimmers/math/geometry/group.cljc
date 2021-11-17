(ns shimmers.math.geometry.group
  "Container for a set of shapes."
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defprotocol IGroup
  (count-children [_]))

(defrecord Group [children])

(extend-type Group
  IGroup
  (count-children [{:keys [children]}]
    (count children))

  g/IBounds
  (bounds [{:keys [children]}]
    (gu/coll-bounds children))
  (width [_]
    (g/width (g/bounds _)))
  (height [_]
    (g/height (g/bounds _)))

  g/ICenter
  (center
    ([_]
     (g/translate _ (tm/- (g/centroid _))))
    ([_ o]
     (g/translate _ (tm/- o (g/centroid _)))))
  ;; Not sure if strictly correct centroid, averages centroids of children
  (centroid [{:keys [children]}]
    (gu/centroid (map g/centroid children)))

  g/IScale
  (scale [_ s]
    (Group. (mapv #(g/scale % s) (get _ :children))))

  g/ITranslate
  (translate [{:keys [children]} t]
    (Group. (mapv (fn [s] (g/translate s t)) children))))

(defn group
  [children]
  (Group.
   (cond (sequential? children)
         children
         (nil? children)
         []
         :else
         [children])))

(defn fit-grid [n {:keys [rows cols]}]
  (cond (and rows cols)
        [cols rows (- (* cols rows) n)]
        (and rows (not cols))
        (fit-grid n {:rows rows
                     :cols (tm/ceil (/ n rows))})
        (and (not rows) cols)
        (fit-grid n {:rows (tm/ceil (/ n cols))
                     :cols cols})
        :else
        ;; This is column major, should it support optimizing for rows first as well?
        (let [cols (tm/ceil (Math/sqrt n))
              rows (tm/ceil (/ n cols))]
          (fit-grid n {:rows rows :cols cols}))))

;; TODO: support odd grids like 4:3:4 and the like
(defn tile-grid
  ([bounds shape-groups] (tile-grid bounds shape-groups {:scale 0.9}))
  ([bounds shape-groups {:keys [scale] :as opts}]
   (let [n (count shape-groups)
         [rows cols _] (fit-grid n opts)
         tiles (take (min n (* rows cols))
                     (g/subdivide bounds {:cols cols :rows rows}))]
     (group (mapcat (fn [group tile]
                      (-> tile
                          (g/scale-size scale)
                          (gu/fit-all-into-bounds (:children group))))
                    shape-groups tiles)))))
