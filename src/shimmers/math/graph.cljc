(ns shimmers.math.graph
  (:require
   [loom.attr :as lga]
   [loom.graph :as lg]
   [shimmers.common.sequence :as cs]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils.intersect :as gisec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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

(defn edge-clips-node-radius?
  "Check if `p` - `q` intersects a node radius in graph `g`.

  attr `:radius` is used to represent the radius of a node."
  [g p q]
  (some (fn [node] (let [r (lga/attr g node :radius)]
                    (when-let [hit (gisec/intersect-ray-sphere? p (tm/- q p) node r)]
                      (< (g/dist p q) (first hit)))))
        (disj (set (lg/nodes g)) p q)))

(defn edges->labeled-graph [edges]
  (let [nodes (vec (set (mapcat identity edges)))
        nodes->idx (into {} (map-indexed (fn [i n] [n (str "n" i)]) nodes))]
    (as-> (lg/weighted-graph) graph
      (reduce-kv (fn [g point idx]
                   (-> g
                       (lg/add-nodes idx)
                       (lga/add-attr idx :position point)))
                 graph nodes->idx)
      (reduce (fn [g [a b]]
                (lg/add-edges g [(nodes->idx a) (nodes->idx b) (g/dist a b)]))
              graph edges))))

(comment (edges->labeled-graph [[(gv/vec2 0 0) (gv/vec2 0 1)]
                                [(gv/vec2 0 1) (gv/vec2 0 2)]]))

(defn position [graph node]
  (lga/attr graph node :position))

(defn heaviest-edge [graph]
  (apply max-key (partial lg/weight graph) (lg/edges graph)))

