(ns shimmers.sketches.flower-petals
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (q/background 1.0)
  (q/stroke 0.0)
  (q/begin-shape)
  (doseq [theta (range 0 eq/TAU 0.05)]
    (let [r (cq/rel-h 0.4)
          p (v/+polar (cq/rel-vec 0.5 0.5) (* r (Math/cos (* 5 theta))) theta)
          [x y] p]
      (q/curve-vertex x y)))
  (q/end-shape))

(sketch/defquil flower-petals
  :created-at "2023-01-09"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
