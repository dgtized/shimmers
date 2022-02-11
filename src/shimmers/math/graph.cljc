(ns shimmers.math.graph
  (:require
   [loom.graph :as lg]
   [shimmers.math.geometry.intersection :as isec]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   [shimmers.common.sequence :as cs]))

;; *probably* need to move position into a graph attribute so that nodes can move.
;; This would adjust distance calculation and generation

(defn edges->graph [edges]
  (reduce (fn [g [a b]] (lg/add-edges g [a b (g/dist a b)]))
          (lg/weighted-graph) edges))

(defn points->graph [points]
  (edges->graph (cs/all-pairs points)))

(defn unique-edges [edges]
  (reduce (fn [edge-set [p q]]
            (if (contains? edge-set [q p])
              edge-set
              (conj edge-set [p q])))
          #{}
          edges))

;; FIXME: still seeing occasional examples where this fails?
(defn planar-edge?
  "Check if edge `p`-`q` in `graph` is planar.

  The edge is planar if all intersections with existing edges are at `p` or `q`."
  [graph p q]
  (every? (fn [[a b]]
            (let [isec (isec/segment-intersect [p q] [a b])]
              (or (and isec (or (tm/delta= isec p) (tm/delta= isec q)))
                  true)))
          (unique-edges (lg/edges graph))))
