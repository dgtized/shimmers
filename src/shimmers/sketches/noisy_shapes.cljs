(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.circle :as tc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]))

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
  (repeatedly n #(geometry/rotate-around-centroid
                  (geom/translate brush (random-position))
                  (q/random 0 Math/PI))))

(defn sample-shape [shape brush fill-density edge-density]
  (doseq [copy
          (concat (generate-strokes brush #(geom/random-point-inside shape) fill-density)
                  (generate-strokes brush #(geom/random-point shape) edge-density))]
    (draw-polygon copy)))

(defn fuzzy-shape [shape fill fill-density edge-density]
  (apply q/fill fill)
  (sample-shape shape (random-brush) fill-density edge-density))

(defn draw []
  (q/background 255)
  (q/no-stroke)
  ;; (q/no-loop)
  ;; (q/background 255 0.1)
  (let [w (q/width)
        h (q/height)
        shape1 (rect/rect (* 0.1 w) (* 0.25 h) (* 0.3 w) (* 0.4 h))
        shape2 (-> (rect/rect (* 0.55 w) (* 0.2 h) (* 0.3 w) (* 0.4 h))
                   (geometry/rotate-around-centroid 0.2))
        shape3 (rect/rect (* 0.35 w) (* 0.5 h) (* 0.3 w) (* 0.4 h))
        shapes [[shape3 [105 0.5 0.5 0.2]
                 (q/random 500 1300) 200]
                [shape1 [10 0.5 0.5 0.2]
                 (q/random 600 2000) 200]
                [shape2 [210 0.5 0.5 0.2]
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
