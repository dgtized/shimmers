(ns shimmers.sketches.impressions-of-open-space
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.quil :as cq]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn scribble [points t]
  (q/curve-tightness t)
  (cq/draw-curve-path (map cq/rel-pos points)))

(defn curly-line [a b]
  (for [t (dr/random-sample 0.4 (range 0 1.0 0.03))
        :let [point (tm/mix a b t)]]
    (if (dr/chance (* 0.08 t))
      (tm/+ point (dr/randvec2 0.05))
      point)))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsl 1.0)
  {})

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 0.15)
  (q/stroke 0 0.4 0.3 0.9)
  ;; landscape
  (doseq [y (range 0.01 0.9 0.008)]
    (scribble (curly-line (gv/vec2 0 y) (gv/vec2 1.0 (+ 0.11 y))) 4.0))
  (q/stroke 0.3 0.4 0.3 0.9)
  (doseq [y (range 0.01 0.9 0.02)]
    (scribble (curly-line (gv/vec2 0 (+ 0.3 y)) (gv/vec2 1.0 (- y 0.1))) 4.0))
  (q/no-stroke)
  ;; field panes
  (q/fill 0.35 0.6 0.8 0.2)
  (q/rect (cq/rel-w 0.3) (cq/rel-h 0.15) (cq/rel-w 0.25) (cq/rel-h 0.1))
  (q/rect (cq/rel-w 0.2) (cq/rel-h 0.35) (cq/rel-w 0.2) (cq/rel-h 0.15))
  (q/rect (cq/rel-w 0.1) (cq/rel-h 0.6) (cq/rel-w 0.15) (cq/rel-h 0.1))
  ;; water panes
  (q/fill 0.52 0.6 0.8 0.2)
  (q/rect (cq/rel-w 0.6) (cq/rel-h 0.4) (cq/rel-w 0.3) (cq/rel-h 0.15))
  (q/rect (cq/rel-w 0.5) (cq/rel-h 0.6) (cq/rel-w 0.25) (cq/rel-h 0.1))
  (q/rect (cq/rel-w 0.4) (cq/rel-h 0.8) (cq/rel-w 0.2) (cq/rel-h 0.15))
  ;; "trees"
  (q/stroke 0.3 0.4 0.2 0.8)
  (q/stroke-weight 1)
  (doseq [point (repeatedly 128 dr/random-vertex)]
    (q/line (cq/rel-pos point)
            (cq/rel-pos (tm/+ point (gv/vec2 0 (dr/random -0.015 0)))))))

(sketch/defquil impressions-of-open-space
  :created-at "2021-03-09"
  :tags #{:static :deterministic}
  :size [900 600]
  :setup setup
  :draw draw
  :middleware [m/fun-mode])
