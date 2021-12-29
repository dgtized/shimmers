(ns shimmers.sketches.lifecycle-of-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shapes [(rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.3 0.3))
                (rect/rect (cq/rel-vec 0.25 0.5) (cq/rel-vec 0.4 0.8))
                (rect/rect (cq/rel-vec 0.6 0.6) (cq/rel-vec 0.9 0.9))]]
    {:shapes shapes}))

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.5)
  (doseq [s shapes]
    (cq/draw-shape (g/vertices s))))

(sketch/defquil lifecycle-of-shapes
  :created-at "2021-12-28"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
