(ns shimmers.algorithm.random-graph
  (:require
   [clojure.set :as set]
   [loom.alg :as la]
   [loom.graph :as lg]
   [shimmers.common.sequence :as cs]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.graph :as graph]
   [thi.ng.geom.core :as g]))

(defn planar [points]
  (let [all-edges (cs/all-pairs points)
        mst (la/prim-mst (graph/edges->graph all-edges))
        mst-edges (set (lg/edges mst))
        possible-edges (set/difference (set all-edges) mst-edges)]
    (reduce (fn [g [a b]]
              (if (and (or (< (min (lg/out-degree g a) (lg/out-degree g b)) 2)
                           (dr/chance 0.2))
                       (graph/planar-edge? g a b))
                (lg/add-edges g [a b (g/dist a b)])
                g))
            mst
            (dr/shuffle possible-edges))))
