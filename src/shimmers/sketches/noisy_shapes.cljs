(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as poly]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.circle :as tc]))

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :hsl 360 1.0 1.0 1.0))

(defn update-state [state]
  state)

(defn draw-polygon [poly]
  (q/begin-shape)
  (doseq [p (geom/vertices poly)]
    (apply q/vertex p))
  (q/end-shape :close))

(defn rotate-around-centroid [polygon t]
  (-> polygon
      (geom/center (gv/vec2 0 0))
      (geom/rotate t)
      (geom/translate (geom/centroid polygon))))

(defn right-angle [s]
  (gt/triangle2 (gv/vec2 0 0)
                (gv/vec2 (* s 19) 0)
                (gv/vec2 0 (* s 23))))

(defn small-rectangle [s]
  (rect/rect 0 0 (* s 13) (* s 17)))

(defn random-brush []
  (let [brushes [
                 #(small-rectangle (q/random 0.4 1.1))
                 #(right-angle (q/random 0.4 1.1))
                 #(tc/circle (q/random 3 8))
                 ]]
    ((rand-nth brushes))))

(defn generate-strokes [brush random-position n]
  (repeatedly n #(rotate-around-centroid
                  (geom/translate brush (random-position))
                  (q/random 0 Math/PI))))

;; FIXME: random-point-inside is not implemented for Polygon2
;; See https://blogs.sas.com/content/iml/2020/10/21/random-points-in-polygon.html
;; or https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
;; ie tesselate convex Polygon, and sample uniformly per triangle

;; If this is implemented, rectangle can be rotated/translated and still use
;; fuzzy shapes.

(defn sample-shape [shape brush fill-density edge-density]
  (doseq [copy
          (concat (generate-strokes brush #(geom/random-point-inside shape) fill-density)
                  (generate-strokes brush #(geom/random-point shape) edge-density))]
    (draw-polygon copy)))

(defn fuzzy-shape [shape brush fill fill-density edge-density]
  (apply q/fill fill)
  (sample-shape shape brush fill-density edge-density))

(defn draw []
  (q/background 255)
  (q/no-stroke)
  ;; (q/no-loop)
  ;; (q/background 255 0.1)
  (let [w (q/width)
        h (q/height)
        shape1 (rect/rect (* 0.1 w) (* 0.25 h) (* 0.3 w) (* 0.4 h))
        shape2 (rect/rect (* 0.6 w) (* 0.15 h) (* 0.3 w) (* 0.4 h))
        shape3 (rect/rect (* 0.35 w) (* 0.5 h) (* 0.3 w) (* 0.4 h))
        brush (random-brush)
        shapes [[shape3 brush [105 0.5 0.5 0.2]
                 (q/random 500 1300) 200]
                [shape1 brush [10 0.5 0.5 0.2]
                 (q/random 600 2000) 200]
                [shape2 brush [210 0.5 0.5 0.2]
                 (q/random 600 2000) 200]
                ]]
    (doseq [args (shuffle shapes)]
      (apply fuzzy-shape args))))


(defn ^:export run-sketch []
  (q/defsketch noisy-shapes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
