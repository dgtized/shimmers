(ns shimmers.sketches.spin-doctor
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.core :as g]
   [shimmers.math.deterministic-random :as dr]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:center (cq/rel-vec 0.5 0.5)
   :radius (cq/rel-h 0.3)
   :destination (cq/rel-vec 0.8 0.3)})

(defn update-state [{:keys [center destination] :as state}]
  (let [p (tm/mix center destination 0.01)]
    (assoc state
           :center p
           :destination (if (< (g/dist destination p) 10.0)
                          (cq/rel-vec (dr/random 0.2 0.8)
                                      (dr/random 0.2 0.8))
                          destination))))

(defn draw [{:keys [center radius]}]
  (q/background 1.0 0.1)
  (cq/circle center radius))

(sketch/defquil spin-doctor
  :created-at "2022-10-29"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
