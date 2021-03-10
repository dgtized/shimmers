(ns shimmers.sketches.scribbles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn scribble [points]
  (q/stroke-weight 0.3)
  (q/curve-tightness 4.0)
  (q/begin-shape)
  (doseq [p points]
    (q/curve-vertex (cq/rel-w (:x p)) (cq/rel-h (:y p))))
  (q/end-shape))

(defn random-vertex []
  (gv/vec2 (rand) (rand)))

(defn curly-line [a b]
  (for [point (map #(tm/mix a b %) (range 0 1.0 0.05))]
    (if (p/chance 0.2)
      (tm/+ point (gv/randvec2 0.05))
      point)))

(defn setup []
  (q/frame-rate 1)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 255)
  (doseq [y (range 0.1 0.9 0.02)]
    (scribble (curly-line (gv/vec2 0 y) (gv/vec2 1.0 y)))))

(defn ^:export run-sketch []
  ;; 20210309
  (q/defsketch scribbles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
