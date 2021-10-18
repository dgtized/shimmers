(ns shimmers.sketches.spiral-distance
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn log-spiral [alpha k theta]
  (let [nat (* alpha (Math/exp (* k theta)))]
    (gv/vec2 (* nat (Math/cos theta))
             (* nat (Math/sin theta)))))

(defn noise-displace [factor r t p]
  (let [[x y] (tm/* p factor)
        n (q/noise x y t)]
    (tm/+ p (v/polar r (* tm/TWO_PI n)))))

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
        points (for [theta (range 0 (* Math/PI 13) 0.2)]
                 (log-spiral 0.1 0.25 (+ theta (tm/fract t))))
        points (mapv (partial noise-displace (/ 1 400) 10 t) points)]
    (q/begin-shape)
    (doseq [[i p] (map-indexed vector points)]
      (when-let [q (nth points (- i 28) nil)]
        (q/line p q))
      (apply q/vertex p))
    (q/end-shape)))

(sketch/defquil spiral-distance
  :created-at "2021-10-18"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
