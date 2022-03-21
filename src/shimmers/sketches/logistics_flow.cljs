(ns shimmers.sketches.logistics-flow
  (:require
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.graph :as graph]
   [shimmers.sketch :as sketch :include-macros true]))

(defn make-graph []
  (let [src (cq/rel-vec 0.1 0.5)
        dst (cq/rel-vec 0.9 0.5)
        ta (cq/rel-vec 0.25 0.3)
        la (cq/rel-vec 0.25 0.7)
        tc (cq/rel-vec 0.5 0.25)
        mc (cq/rel-vec 0.5 0.5)
        lc (cq/rel-vec 0.5 0.75)
        tb (cq/rel-vec 0.75 0.3)
        lb (cq/rel-vec 0.75 0.7)
        edges [[src ta]
               [src la]
               [ta tc]
               [ta mc]
               [la lc]
               [la mc]
               [mc tb]
               [tc tb]
               [mc lb]
               [lc lb]
               [tb dst]
               [lb dst]]]
    (graph/edges->graph edges)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:graph (make-graph)})

(defn update-state [state]
  state)

(defn draw [{:keys [graph]}]
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0)
  (doseq [p (lg/nodes graph)]
    (cq/circle p 4.0))
  (q/no-fill)
  (q/stroke 0.0)
  (q/stroke-weight 0.8)
  (doseq [[p q] (lg/edges graph)]
    (q/line p q)))

(sketch/defquil logistics-flow
  :created-at "2022-03-20"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
