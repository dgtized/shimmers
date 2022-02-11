(ns shimmers.math.graph
  (:require
   [loom.graph :as lg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils.intersect :as gisec]
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

(defn planar-edge?
  "Check if edge `p`-`q` in `graph` is planar.

  The edge is planar iff intersections with existing edges are at `p` or `q`."
  [graph p q]
  (or (lg/has-edge? graph p q)
      (lg/has-edge? graph q p)
      (not-any? (fn [[a b]]
                  (let [{type :type isec :p isec2 :q} (gisec/intersect-line2-line2? p q a b)]
                    (or
                     ;; intersecting somewhere other than p or q
                     (and (= :intersect type) (not (or (tm/delta= isec p) (tm/delta= isec q))))
                     ;; coincident at more than one point
                     (and (= :coincident type) (not (tm/delta= isec isec2))))))
                (unique-edges (lg/edges graph)))))
