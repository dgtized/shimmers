(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.circle :as tc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.math.probability :as p]))

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :hsl 360 1.0 1.0 1.0))

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

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

(defn random-displace [shapes p offset]
  (let [sampling (random-sample p shapes)
        remaining (remove (set sampling) shapes)]
    (concat remaining
            (for [shape sampling]
              (geom/translate shape (tm/* offset (rand)))))))

(defn sample-shape [shape brush fill-density edge-density]
  (concat (generate-strokes brush #(geom/random-point shape) edge-density)
          (generate-strokes brush #(geom/random-point-inside shape) fill-density)))

(defn fuzzy-shape [shape fill {:keys [fill-density edge-density displacement]
                               :or {fill-density (q/random 500 2500)
                                    edge-density 200
                                    displacement (if (p/chance 0.5) (* 0.25 (rand)) 0)}}]
  (apply q/fill fill)
  (doseq [poly (-> (sample-shape shape (random-brush) fill-density edge-density)
                   (random-displace displacement
                                    (gv/vec2 0 (* 0.5 (q/height)))))]
    (draw-polygon poly)))

(defn draw []
  (q/background 255)
  (q/no-stroke)
  ;; (q/no-loop)
  ;; (q/background 255 0.1)
  (let [w (q/width)
        h (q/height)
        shape1 (rect/rect (* 0.1 w) (* 0.15 h) (* 0.3 w) (* 0.4 h))
        shape2 (-> (rect/rect (* 0.55 w) (* 0.1 h) (* 0.3 w) (* 0.4 h))
                   (geometry/rotate-around-centroid 0.2))
        shape3 (rect/rect (* 0.35 w) (* 0.4 h) (* 0.3 w) (* 0.4 h))
        shapes [[shape1 [10 0.5 0.5 0.2]]
                [shape2 [210 0.5 0.5 0.2]]
                [shape3 [105 0.5 0.5 0.2]]]]
    (doseq [args (shuffle shapes)]
      (apply fuzzy-shape args))))


(defn ^:export run-sketch []
  (q/defsketch noisy-shapes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
