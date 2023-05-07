(ns shimmers.sketches.logistics-flow
  (:require
   [loom.alg :as la]
   [loom.attr :as lga]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-graph :as rg]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.graph :as graph]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-graph [bounds]
  (let [src[0.1 0.5]
        dst[0.9 0.5]
        ta [0.25 0.3]
        la [0.25 0.7]
        tc [0.5 0.25]
        c1 [0.5 0.4]
        c2 [0.5 0.6]
        lc [0.5 0.75]
        tb [0.75 0.3]
        lb [0.75 0.7]
        edges [[src ta]
               [src la]
               [ta tc]
               [ta c1]
               [la lc]
               [la c2]
               [c1 c2]
               [c1 tb]
               [tc tb]
               [c2 lb]
               [lc lb]
               [tb dst]
               [lb dst]]]
    (->> edges
         (mapv (fn [pair] (mapv #(g/unmap-point bounds (gv/vec2 %)) pair)))
         graph/edges->graph)))

(comment (let [g (lg/weighted-digraph (make-graph (rect/rect 10)))]
           (la/max-flow g (gv/vec2 [1 5]) (gv/vec2 [9 5]))))

(defn extreme-edges [g]
  (let [node-pos (map (fn [n] [(graph/position g n) n]) (lg/nodes g))
        sorted (sort-by (fn [[[x _] _]] x) node-pos)]
    (map second [(first sorted) (last sorted)])))

(defn annotate-flow [graph flow]
  (reduce (fn [g [from outbound]]
            (reduce (fn [g' [to cost]]
                      (lga/add-attr g' from to :cost cost))
                    g outbound))
          graph flow))

;; I think subgraph/flygraph does this too but couldn't get it working
(defn map-edges [f graph]
  (let [g' (reduce (fn [g [a b]] (lg/add-edges g (f g a b)))
                   (if (lg/directed? graph)
                     (lg/weighted-digraph)
                     (lg/weighted-graph))
                   (lg/edges graph))]
    (if-let [attrs (:attrs graph)]
      (assoc g' :attrs attrs)
      g')))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n 12
        points (rp/poisson-disc-sampling (cq/screen-rect 0.9) n)
        graph (lg/weighted-digraph (rg/voronoi (take n (dr/shuffle points))))
        [src dst] (extreme-edges graph)
        heaviest (graph/heaviest-edge graph)
        max-edge (apply lg/weight graph heaviest)
        g (map-edges (fn [_ a b]
                       (let [rel-weight (/ (lg/weight graph a b) (+ 2 max-edge))]
                         [a b (int (* 10 (- 1 rel-weight)))]))
                     graph)
        [flow max-flow] (la/max-flow g src dst)]
    {:graph (annotate-flow g flow)
     :src src
     :dst dst
     :max-flow max-flow}))

(defn update-state [state]
  state)

(defn draw [{:keys [graph src dst]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0)
  (q/text-size 8)
  (doseq [node (lg/nodes graph)
          :let [p (graph/position graph node)
                [x y] (tm/+ p (gv/vec2 8 12))]]
    (cq/circle p 3.0)
    (q/text node x y))
  (doseq [[p q] (lg/edges graph)
          :let [pos-p (graph/position graph p)
                pos-q (graph/position graph q)
                [x y] (tm/mix pos-p pos-q 0.33)
                cost (lga/attr graph p q :cost)
                capacity (lg/weight graph p q)]]
    (when (> cost 0)
      (q/stroke-weight (+ 2 (* 4 (/ cost capacity))))
      (q/stroke 0.35 0.5 0.5 0.35)
      (q/line pos-p pos-q))
    (q/stroke-weight 0.75)
    (q/stroke 0.0)
    (q/line pos-p pos-q)
    (q/no-stroke)
    (q/text (str p "-" q " " (int cost) "/" (int capacity)) x y))

  (q/no-fill)
  (q/stroke-weight 1.5)
  (q/stroke 0.0 0.5 0.5 0.75)
  (cq/circle (graph/position graph src) 5.0)
  (q/stroke 0.75 0.7 0.5 0.75)
  (cq/circle (graph/position graph dst) 5.0))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition logistics-flow
  {:created-at "2022-03-20"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
