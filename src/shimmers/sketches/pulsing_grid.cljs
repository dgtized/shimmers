(ns shimmers.sketches.pulsing-grid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.rect :as rect]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [r (rect/rect (cq/rel-w 0.05) (cq/rel-h 0.35)
                     (cq/rel-w 0.90) (cq/rel-h 0.30))
        cells (geom/subdivide r {:rows 6 :cols 24})]
    {:cells (for [cell cells]
              (geom/scale-size cell 0.9))}))

(defn update-state [state]
  state)

(defn draw [{:keys [cells]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [cell cells]
    (cq/rectangle cell)))

(sketch/defquil pulsing-grid
  :created-at "2021-08-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
