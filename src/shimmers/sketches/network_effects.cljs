(ns shimmers.sketches.network-effects
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn top-n-neighbors [n from nodes]
  (take n (sort-by #(g/dist from %) (remove #{from} nodes))))

(defn neighborhood [n nodes]
  (mapcat (fn [node] (map vector
                         (repeatedly (constantly node))
                         (top-n-neighbors 3 node nodes)))
          nodes))

(defn force-push [n nodes]
  (for [node nodes
        :let [neighborhood (top-n-neighbors n node nodes)
              average (reduce tm/+
                              node
                              (map (fn [n]
                                     (let [d (g/dist node n)]
                                       (tm/* (tm/- node n) (/ 9.8 (* d d)))))
                                   neighborhood))]]
    (tm/mix node average 0.1)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [screen (g/center (cq/screen-rect 0.8) (cq/rel-vec 0.5 0.5))
        nodes (repeatedly 16 #(g/random-point-inside screen))]
    {:nodes nodes
     :connections (neighborhood 3 nodes)
     :pings []}))

(defn update-state [{:keys [nodes] :as state}]
  (let [nodes' (force-push 3 nodes)]
    (assoc state
           :nodes nodes'
           :connections (neighborhood 3 nodes'))))

(defn draw [{:keys [nodes connections]}]
  (q/background 1.0 0.05)
  (q/stroke-weight 0.5)
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
