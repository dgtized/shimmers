(ns shimmers.sketches.scribbles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn scribble [points]
  (q/stroke-weight 0.2)
  (q/stroke 0 0 0 0.9)
  (q/curve-tightness 4.0)
  (q/begin-shape)
  (doseq [p points]
    (q/curve-vertex (cq/rel-w (:x p)) (cq/rel-h (:y p))))
  (q/end-shape))

(defn random-vertex []
  (gv/vec2 (rand) (rand)))

(defn curly-line [a b]
  (for [t (remove #(p/chance 0.6) (range 0 1.0 0.03))
        :let [point (tm/mix a b t)]]
    (if (p/chance (* 0.05 t))
      (tm/+ point (gv/randvec2 0.05))
      point)))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 255)
  (doseq [y (range 0.01 0.9 0.008)]
    (scribble (curly-line (gv/vec2 0 y) (gv/vec2 1.0 (+ 0.1 y)))))
  (doseq [y (range 0.01 0.9 0.02)]
    (scribble (curly-line (gv/vec2 0 (+ 0.3 y)) (gv/vec2 1.0 (- y 0.1))))))

(defn ^:export run-sketch []
  ;; 20210309
  (q/defsketch scribbles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
