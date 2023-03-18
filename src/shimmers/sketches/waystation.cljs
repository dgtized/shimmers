(ns shimmers.sketches.waystation
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n 2]
    {:tracks (cs/midsection (tm/norm-range (inc n)))}))

(defn update-state [state]
  state)

(defn draw-track [offset]
  (let [th 0.02]
    (q/line (cq/rel-vec 0 (- offset th)) (cq/rel-vec 1.0 (- offset th)))
    (q/line (cq/rel-vec 0 (+ offset th)) (cq/rel-vec 1.0 (+ offset th)))
    (doseq [x (range 0.01 1 0.015)]
      (q/line (cq/rel-vec x (- offset (* th 1.2))) (cq/rel-vec x (+ offset (* th 1.2)))))))

(defn draw [{:keys [tracks]}]
  (q/background 1.0)
  (doseq [track tracks]
    (draw-track track)))

(sketch/defquil waystation
  :created-at "2023-05-17"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
