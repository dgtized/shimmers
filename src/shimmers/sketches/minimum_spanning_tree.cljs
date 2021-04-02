(ns shimmers.sketches.minimum-spanning-tree
  (:require [nifty.disjoint-set :as djs]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [tailrecursion.priority-map :refer [priority-map]]
            [thi.ng.geom.core :as geom]))

(defn distances [v points]
  (reduce (fn [m p] (assoc m p (geom/dist v p)))
          {} points))

(defn ranked-edges [points]
  (->> (for [[u v] (cs/all-pairs points)]
         [(geom/dist u v) [u v]])
       (sort-by first)
       (map second)))

;; Something off about performance here. Points to edges is <N^2, but pretty
;; close, so maybe sort x/y and find close somehow? On top of that though,
;; kruskal-step is not performing as quickly as prim's.
(defn kruskal-mst [points]
  (letfn [(kruskal-step [forest edges union-set]
            (if (empty? edges)
              forest
              (let [[[u v] & remaining] edges]
                (if (not= (djs/canonical union-set u)
                          (djs/canonical union-set v))
                  (recur (conj forest [u v])
                         remaining
                         (djs/union union-set u v))
                  (recur forest remaining union-set)))))]
    (kruskal-step [] (ranked-edges points)
                  (apply djs/disjoint-set points))))

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

(defn prim-mst [points]
  (letfn [(prim-step [forest vertices weights best-edge]
            (if (empty? vertices)
              forest
              (let [[v _] (first weights)
                    edge (get best-edge v)
                    remaining (disj vertices v)
                    [weights' best-edge']
                    (prim-update v remaining (dissoc weights v) (dissoc best-edge v))]
                (recur (conj forest edge)
                       remaining
                       weights'
                       best-edge'))))]
    (let [[vertex & remaining] points]
      (prim-step []
                 (set remaining)
                 (apply priority-map (mapcat identity (distances vertex remaining)))
                 (into {} (for [p remaining] {p [vertex p]}))))))

(defn fresh-graph []
  (let [point-gen (rand-nth [(partial q/random 0.05 0.95)
                             (p/gaussian-clamped 0.5 0.15)])
        points (geometry/generate-points 256 point-gen)
        edges ((rand-nth [prim-mst kruskal-mst]) points)]
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
  (doseq [point points
          :let [[x y] (cq/rel-pos point)]]
    (q/ellipse x y 0.2 0.2))
  (q/stroke-weight 0.5)
  (q/stroke 50 50 230)
  (doseq [[p q] (take step edges)]
    (q/line (cq/rel-pos p) (cq/rel-pos q))))

(defn ^:export run-sketch []
  ;; 20210320
  (q/defsketch minimum-spanning-tree
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
