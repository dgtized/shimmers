(ns shimmers.sketches.minimum-spanning-tree
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [tailrecursion.priority-map :refer [priority-map]]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]))

(defn generate-points [n dist]
  (repeatedly n #(gv/vec2 (dist) (dist))))

(defn distances [v points]
  (reduce (fn [m p] (assoc m p (geom/dist v p)))
          {} points))

(defrecord Forest [vertices edges])

(defn prim-update [added vertices weights best-edges]
  (if (empty? vertices)
    [weights best-edges]
    (let [vertex (first vertices)
          dist (geom/dist vertex added)
          prior (get weights vertex)
          better (and prior (< dist prior))]
      (if better
        (recur added (rest vertices)
               (assoc weights vertex dist)
               (assoc best-edges vertex [added vertex]))
        (recur added (rest vertices) weights best-edges)))))

(defn prim-step [forest vertices weights best-edge]
  (if (empty? vertices)
    forest
    (let [[v _] (first weights)
          e (get best-edge v)
          remaining (disj vertices v)
          [weights' best-edge']
          (prim-update v remaining (dissoc weights v) (dissoc best-edge v))]
      (recur (-> forest
                 (update :vertices conj v)
                 (update :edges conj e))
             remaining
             weights'
             best-edge'))))

(defn prim-mst [points]
  (let [[vertex & remaining] points]
    (prim-step (->Forest [vertex] [])
               (set remaining)
               (apply priority-map (mapcat identity (distances vertex remaining)))
               (into {} (for [p remaining] {p [vertex p]})))))

(defn fresh-graph []
  (let [points (generate-points 256 (partial q/random 0.05 0.95))
        mst (prim-mst points)]
    {:points points
     :edges (:edges mst)
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
  (doseq [point points]
    (apply q/point (cq/rel-pos point)))
  (q/stroke-weight 0.5)
  (doseq [[p q] (take step edges)]
    (q/line (cq/rel-pos p) (cq/rel-pos q))))

(defn ^:export run-sketch []
  (q/defsketch minimum-spanning-tree
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
