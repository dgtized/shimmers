(ns shimmers.math.graph
  (:require
   [loom.graph :as lg]
   [shimmers.math.geometry.intersection :as isec]
   [thi.ng.math.core :as tm]))

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
