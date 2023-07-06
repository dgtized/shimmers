(ns shimmers.sketches.minimum-spanning-tree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.minimum-spanning-tree :as mst]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.points :as points]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn fresh-graph []
  (let [point-gen (dr/weighted {(partial dr/random 0.05 0.95) 1
                                (fn [] (tm/clamp01 (dr/gaussian 0.5 0.15))) 1})
        n (dr/weighted {64 1 128 1 256 1})
        points (points/generate n point-gen)
        calculate-tree (dr/rand-nth [mst/prim-points mst/kruskal-points])
        edges (calculate-tree points)]
    {:points points
     :edges edges
     :step 0.0}))

(defn graph-step [{:keys [step edges] :as state}]
  (if (> step (count edges))
    [true state]
    [false (update state :step + 0.2)]))

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
  (doseq [[i [p q]] (map-indexed vector (take (Math/floor (inc step)) edges))
          :let [wp (cq/rel-vec p)
                wq (cq/rel-vec q)]]
    (if (< i (Math/floor step))
      (q/line wp wq)
      (q/line wp (tm/mix wp wq (tm/fract step))))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition minimum-spanning-tree
  {:created-at "2021-03-20"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))
