(ns shimmers.sketches.triangle-flow
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.core :as sm]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn noise-at-p [bounds p t]
  (let [[rx ry] (tm/* (g/map-point bounds p) 2)
        x (sm/reflect-into rx 2)
        y (sm/reflect-into ry 2)]
    (q/noise x y t)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds (cq/screen-rect)
   :t 0})

(defn update-state [state]
  (update state :t + 0.005))

(defn draw [{:keys [bounds t]}]
  (q/background 1.0 0.2)
  (doseq [i (tm/norm-range 45)
          j (tm/norm-range 45)
          :let [p (g/unmap-point bounds (gv/vec2 i j))]]
    (q/line p (tm/+ p (v/polar 10 (* eq/TAU (noise-at-p bounds p t)))))))

(sketch/defquil triangle-flow
  :created-at "2022-04-13"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
