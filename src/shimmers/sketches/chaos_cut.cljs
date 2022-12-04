(ns shimmers.sketches.chaos-cuts
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.line :as gl]
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [thi.ng.geom.core :as g]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.circle :as gc]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.8)]
    {:bounds bounds
     :shapes []
     :t 0}))

(defn cut [line shape]
  (->> (lines/cut-polygon shape line)
       (mapcat (fn [poly]
                 (->> (poly-detect/inset-polygon poly 4)
                      poly-detect/split-self-intersection
                      (filter (fn [inset] (> (g/area inset) 100))))))))

(defn gen-lines [t]
  (let [a (gc/circle (cq/rel-vec 0.5 -2) (cq/rel-h 0.4))
        b (gc/circle (cq/rel-vec 0.5 2) (cq/rel-h 0.35))
        c (gc/circle (cq/rel-vec -2 0.5) (cq/rel-h 0.35))
        d (gc/circle (cq/rel-vec 2 0.5) (cq/rel-h 0.35))]
    [(gl/line2 (g/point-at a (* t 0.01))
               (g/point-at b (* t 0.02)))
     (gl/line2 (g/point-at c (* t 0.03))
               (g/point-at d (* t 0.05)))]))

(defn update-state [{:keys [t bounds] :as state}]
  (let [lines (gen-lines t)]
    (-> state
        (update :t + (dr/random 0.01 0.1))
        (assoc :shapes
               (reduce (fn [polygons line]
                         (mapcat (fn [poly] (cut line poly)) polygons))
                       [bounds]
                       lines)))))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/fill 1.0)
  (doseq [polygon shapes]
    (cq/draw-polygon polygon)))

(sketch/defquil chaos-cuts
  :created-at "2022-12-04"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
