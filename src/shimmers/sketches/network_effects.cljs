(ns shimmers.sketches.network-effects
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as g]))

(defn top-n-neighbors [n from nodes]
  (take n (sort-by #(g/dist from %) (remove #{from} nodes))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [screen (g/center (cq/screen-rect 0.8) (cq/rel-vec 0.5 0.5))
        nodes (repeatedly 16 #(g/random-point-inside screen))]
    {:nodes nodes
     :connections (mapcat (fn [node] (map vector
                                         (repeatedly (constantly node))
                                         (top-n-neighbors 3 node nodes)))
                          nodes)
     :pings []}))

(defn update-state [state]
  state)

(defn draw [{:keys [nodes connections]}]
  (q/background 1.0)
  (q/stroke-weight 0.8)
  (q/no-fill)
  (doseq [n nodes]
    (cq/circle n 3.0))

  (doseq [[p q] connections]
    (q/line p q)))

(sketch/defquil network-effects
  :created-at "2021-12-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
