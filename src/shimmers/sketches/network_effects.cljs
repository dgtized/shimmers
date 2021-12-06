(ns shimmers.sketches.network-effects
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defn neighbors [from nodes]
  (sort-by #(g/dist from %) (remove #{from} nodes)))

(defn intersects? [lines line]
  (some (fn [l] (->> line
                    (g/intersect-line (g/scale-size l 0.99))
                    :type
                    #{:intersect}))
        lines))

(defn neighborhood [n nodes]
  (reduce (fn [conns node]
            (->> nodes
                 (neighbors node)
                 (map gl/line2 (repeatedly (constantly node)))
                 (remove (fn [line] (intersects? conns line)))
                 (take n)
                 (concat conns)))
          []
          nodes))

(defn force-push [n nodes]
  (for [node nodes
        :let [surroundings (take n (neighbors node nodes))
              average (reduce tm/+
                              node
                              (map (fn [n]
                                     (let [d (g/dist node n)]
                                       (tm/* (tm/- node n) (/ 9.8 (* d d)))))
                                   surroundings))]]
    (v/clamp-bounds (g/center (cq/screen-rect 0.8) (cq/rel-vec 0.5 0.5))
                    (tm/mix node average 0.2))))

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

  (doseq [{[p q] :points} connections]
    (q/line p q)))

(sketch/defquil network-effects
  :created-at "2021-12-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
