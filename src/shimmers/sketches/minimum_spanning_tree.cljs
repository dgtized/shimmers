(ns shimmers.sketches.minimum-spanning-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.algorithm.minimum-spanning-tree :as mst]))

(defn fresh-graph []
  (let [point-gen (rand-nth [(partial q/random 0.05 0.95)
                             (p/gaussian-clamped 0.5 0.15)])
        points (geometry/generate-points 256 point-gen)
        edges ((rand-nth [mst/prim mst/kruskal]) points)]
    {:points points
     :edges edges
     :step 0}))

(defn graph-step [{:keys [step edges] :as state}]
  (if (= step (count edges))
    [true state]
    [false (update state :step inc)]))

(defn setup []
  (fresh-graph))

(defn update-state [state]
  (cq/if-steady-state
   state 5
   fresh-graph
   graph-step))

(defn draw [{:keys [points edges step]}]
  (q/background 255)
  (q/stroke-weight 2)
  (q/ellipse-mode :radius)
  (q/stroke 0 0 0)
  (doseq [pt points]
    (cq/circle (cq/rel-pos pt) 0.2))
  (q/stroke-weight 0.5)
  (q/stroke 50 50 230)
  (doseq [[p q] (take step edges)]
    (q/line (cq/rel-pos p) (cq/rel-pos q))))

(sketch/defquil minimum-spanning-tree
  :created-at "2021-03-20"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
