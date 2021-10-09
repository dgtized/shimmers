(ns shimmers.sketches.rtree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.rtree :as rtree]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [circles (repeatedly 1000 #(gc/circle (cq/rel-vec (rand) (rand)) 3.0))]
    {:circles circles
     :rtree (rtree/create circles)}))

(defn update-state [state]
  state)

(defn draw [{:keys [rtree]}]
  (doseq [{:keys [bounds data]} (tree-seq (fn [x] (not-empty (:children x))) :children rtree)]
    (q/stroke 0)
    (cq/rectangle bounds)
    (when-let [{p :p r :r} data]
      (q/stroke 0.0 0.6 0.6 1.0)
      (cq/circle p r))))

(sketch/defquil rtree
  :created-at "2021-10-09"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
