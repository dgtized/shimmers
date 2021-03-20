(ns shimmers.sketches.minimum-spanning-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]))

(defn generate-points [n dist]
  (repeatedly n #(gv/vec2 (dist) (dist))))

(defn all-pairs [points]
  (for [p1 points
        p2 points
        :when (not= p1 p2)]
    [p1 p2]))

(comment (all-pairs (generate-points 2 rand)))

(defn prim-mst [points]
  points)

(defn setup []
  {:points (generate-points 50 (partial q/random 0.05 0.95))})

(defn update-state [state]
  state)

(defn draw [{:keys [points]}]
  (q/stroke-weight 2)
  (doseq [point points]
    (apply q/point (cq/rel-pos point))))

(defn ^:export run-sketch []
  (q/defsketch minimum-spanning-tree
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
