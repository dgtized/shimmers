(ns shimmers.sketches.network-effects
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
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

(defn clamped [bounds nodes]
  (mapv (partial v/clamp-bounds bounds) nodes))

(defn force-directed [nodes connections ideal-dist]
  (let [conns (group-by (comp first :points) connections)
        force-on (fn [node neighbor]
                   (let [dist (g/dist node neighbor)
                         spring (* 0.01 (- ideal-dist dist))]
                     (tm/* (tm/- node neighbor)
                           (/ spring dist))))]
    (for [node nodes
          :let [surroundings (map (comp second :points) (get conns node))
                forces (map (partial force-on node) surroundings)]]
      (tm/mix node (reduce tm/+ node forces) 0.25))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [screen (g/center (cq/screen-rect 0.8) (cq/rel-vec 0.5 0.5))
        nodes (repeatedly 16 #(g/random-point-inside screen))]
    {:bounds screen
     :nodes nodes
     :connections (neighborhood 3 nodes)
     :pings []}))

(defn update-state [{:keys [bounds nodes connections] :as state}]
  (let [nodes' (clamped bounds (force-directed nodes connections (cq/rel-h 0.25)))
        conns (neighborhood 3 nodes')
        polygons (->> conns
                      (mapv :points)
                      poly-detect/edges->graph
                      poly-detect/simple-polygons)]
    (assoc state
           :nodes nodes'
           :connections conns
           :polygons polygons)))

(defn draw [{:keys [bounds nodes connections polygons]}]
  (q/background 1.0 0.2)
  (q/stroke-weight 0.5)
  (q/stroke 0 0.5)
  (q/no-fill)
  (doseq [n nodes]
    (cq/circle n 3.0))

  (doseq [{[p q] :points} connections]
    (q/line p q))

  (q/no-stroke)
  (q/fill 0 0.01)
  (doseq [shape polygons
          :let [poly (gp/inset-polygon shape -5.0)]]
    (when (every? #(g/contains-point? bounds %) poly)
      (cq/draw-shape poly))))

(sketch/defquil network-effects
  :created-at "2021-12-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
