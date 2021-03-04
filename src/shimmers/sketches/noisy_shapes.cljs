(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as poly]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]))

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

(defn right-angle []
  (geom/as-polygon (gt/triangle2 (gv/vec2 0 0)
                                 (gv/vec2 19 0)
                                 (gv/vec2 0 23))))

(defn generate-strokes [brush random-position n]
  (repeatedly n #(rotate-around-centroid
                  (geom/translate brush (random-position))
                  (q/random 0 Math/PI))))

(defn sample-shape [shape brush fill-density edge-density]
  (doseq [copy
          (concat (generate-strokes brush #(geom/random-point-inside shape) fill-density)
                  (generate-strokes brush #(geom/random-point shape) edge-density))]
    (draw-polygon copy)))

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
        brush (right-angle)]
    (q/fill 5 0.5 0.5 0.2)
    (sample-shape shape1 brush
                  (* (+ 0.25 (* 0.5 (rand))) 2500) 400)
    (q/fill 210 0.5 0.5 0.2)
    (sample-shape shape2 brush
                  (* (+ 0.25 (* 0.5 (rand))) 2500) 400)
    (q/fill 120 0.5 0.5 0.2)
    (sample-shape shape3 brush
                  (* (+ 0.25 (* 0.3 (rand))) 2500) 400)))

(defn ^:export run-sketch []
  (q/defsketch noisy-shapes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
