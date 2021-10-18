(ns shimmers.sketches.spiral-distance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]
            [thi.ng.math.core :as tm]))

(defn log-spiral [alpha k theta]
  (let [nat (* alpha (Math/exp (* k theta)))]
    (gv/vec2 (* nat (Math/cos theta))
             (* nat (Math/sin theta)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (q/background 1.0)
  (q/no-fill)
  (q/translate (cq/rel-vec 0.66 0.33))
  (let [t (/ (q/frame-count) 200)
        points (for [theta (range 0 (* Math/PI 12) 0.2)]
                 (log-spiral 0.1 0.25 (- theta (tm/fract t))))]
    (q/begin-shape)
    (doseq [p points]
      (apply q/vertex p))
    (q/end-shape)))

(sketch/defquil spiral-distance
  :created-at "2021-10-18"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
