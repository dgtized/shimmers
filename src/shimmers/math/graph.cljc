(ns shimmers.math.graph
  (:require
   [clojure.set :as set]
   [loom.attr :as lga]
   [loom.graph :as lg]
   [shimmers.common.sequence :as cs]
   [shimmers.math.geometry.intersection :as isec]
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

(defn adjacent-peers [edges]
  (reduce (fn [peers [p q]]
            (-> peers
                (update p (fnil conj #{}) q)
                (update q (fnil conj #{}) p)))
          {} edges))

;; https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
(defn bk-pivot
  "Bron–Kerbosch algorithm finds all maximal cliques in undirected graph.

  `neighbors` is a map of nodes -> neighboring nodes, `r` is the starting clique,
  `p` is the set of all nodes, and `x` is the excluded set already proved not part
  of the clique in `r`."
  [neighbors r p x]
  (if (and (empty? p) (empty? x))
    [(set r)]
    (let [pivot-u (first (concat p x))
          pivot-set (set/difference (set p) (set (neighbors pivot-u)))]
      (loop [p p
             pivot-set pivot-set
             x x
             result []]
        (if (empty? pivot-set)
          result
          (let [v (first pivot-set)
                v-neighbors (set (neighbors v))]
            (recur (rest p) (rest pivot-set) (conj x v)
                   (into result
                         (bk-pivot neighbors
                                   (conj r v)
                                   (set/intersection (set p) v-neighbors)
                                   (set/intersection (set x) v-neighbors))))))))))

(defn cliques [neighbors nodes]
  (bk-pivot neighbors #{} (set nodes) #{}))

(defn index-graph [pts]
  {:points (vec pts)
   :vertex-index (zipmap pts (range (count pts)))
   :vertices (mapv (fn [v] (str "v" v)) (range (count pts)))})

;; TODO: should edges be imported by points or by vertices?
(comment (index-graph [(gv/vec2 0 1) (gv/vec2 1 0)]))

(defn crossing-segment? [segment]
  (fn [line]
    (when-let [p (isec/segment-intersect line segment)]
      (let [[a b] line]
        (when (and (not (tm/delta= a p)) (not (tm/delta= b p)))
          line)))))

(defn planar-edges
  ([edges]
   (planar-edges
    (fn [graph edge] (not-any? (crossing-segment? edge) graph))
    edges))
  ([planar? edges]
   (reduce
    (fn [graph edge]
      (if (planar? graph edge)
        (conj graph edge)
        graph))
    []
    edges)))
