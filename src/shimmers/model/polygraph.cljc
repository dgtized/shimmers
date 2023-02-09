(ns shimmers.model.polygraph
  "a graph of points where each node is uniquely identified but also maps to an updatable position."
  (:require
   [loom.attr :as lga]
   [loom.graph :as lg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defn nodes->points [graph]
  (->> graph
       lg/nodes
       (map (fn [node] [node (lga/attr graph node :pos)]))
       (into {})))

;; FIXME: awful performance, makes creating a graph 2*N^2, need spatial index or hashing
(defn point->node
  [graph point]
  (let [points (nodes->points graph)
        tolerance 0.01]
    (or (some (fn [[node pos]]
                (when (< (g/dist-squared point pos) tolerance)
                  node))
              points)
        (count points))))

(defn add-edge [graph [p q]]
  (let [a (point->node graph p)
        b (point->node graph q)]
    (-> graph
        (lg/add-edges [a b (g/dist p q)])
        (lga/add-attr a :pos p)
        (lga/add-attr b :pos q))))

(defn edgegraph [edges]
  (reduce add-edge (lg/weighted-graph) edges))

(comment
  (edgegraph [[(gv/vec2 0 0) (gv/vec2 10 0)]
              [(gv/vec2 10 0) (gv/vec2 0 10)]
              [(gv/vec2 0 10) (gv/vec2 0 0)]])

  (defn polygraph [_edges])
  (defn pointgraph [_points]))
