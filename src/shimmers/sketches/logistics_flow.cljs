(ns shimmers.sketches.logistics-flow
  (:require
   [loom.alg :as la]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-graph :as rg]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
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

(comment (let [g (make-graph (rect/rect 10))]
           (la/max-flow (lg/weighted-digraph g) (gv/vec2 [1 5]) (gv/vec2 [9 5]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n 15
        points (rp/poisson-disc-sampling (cq/screen-rect 0.9) n)]
    {:graph (rg/voronoi (take n (dr/shuffle points)))}))

(defn update-state [state]
  state)

(defn draw [{:keys [graph]}]
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0)
  (doseq [node (lg/nodes graph)
          :let [p (graph/position graph node)
                [x y] (tm/+ p (gv/vec2 8 12))]]
    (cq/circle p 4.0)
    (q/text node x y))
  (q/stroke-weight 0.6)
  (doseq [[p q] (lg/edges graph)
          :let [pos-p (graph/position graph p)
                pos-q (graph/position graph q)
                [x y] (tm/mix pos-p pos-q 0.66)]]
    (q/stroke 0.0)
    (q/line pos-p pos-q)
    (q/no-stroke)
    (q/text (str p "-" q) x y)))

(sketch/defquil logistics-flow
  :created-at "2022-03-20"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
