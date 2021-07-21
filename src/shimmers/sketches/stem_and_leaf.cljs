(ns shimmers.sketches.stem-and-leaf
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [(gc/circle (cq/rel-pos 0.5 0.5) (cq/rel-h 0.1))
             (assoc (gc/circle (cq/rel-pos 0.2 0.5) (cq/rel-h 0.15)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.8 0.5) (cq/rel-h 0.05)) :parent 0)]})

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r]} circles]
    (cq/circle p r))
  (doseq [{:keys [p r parent]} circles
          :when parent
          :let [{p' :p r' :r} (nth circles parent)]]
    (q/line (tm/+ p (gv/vec2 0 r)) (tm/+ p' (gv/vec2 0 r')))
    (q/line (tm/- p (gv/vec2 0 r)) (tm/- p' (gv/vec2 0 r')))))

(sketch/defquil stem-and-leaf
  :created-at "2021-07-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
