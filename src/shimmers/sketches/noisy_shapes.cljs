(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.sequence :as cs]
            [thi.ng.geom.circle :as tc]
            [thi.ng.geom.core :as geom]
            thi.ng.geom.polygon
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            ;; [thi.ng.math.core :as tm]
            ;; [thi.ng.geom.utils :as gu]
            [thi.ng.geom.types :refer [Polygon2]]))

;; Modified from https://github.com/clojure/data.generators/blob/master/src/main/clojure/clojure/data/generators.clj#L73
;; not available for clojurescript
(defn random-weighted
  "Given a map of generators and weights, return a value from one of
   the generators, selecting generator based on weights."
  [m]
  (let [weights   (reductions + (vals m))
        total   (last weights)
        choices (map vector (keys m) weights)
        choice (* total (rand))]
    (loop [[[c w] & more] choices]
      (when w
        (if (< choice w)
          c
          (recur more))))))

(comment
  (random-weighted {:a 0.2 :b 0.8}))

(defn random-weighted-by [f xs]
  (random-weighted (cs/mapping f xs)))

(comment (random-weighted-by inc [1 2 3]))

(extend-type Polygon2
  geom/ISample
  (random-point-inside
    [_] (let [triangles (map gt/triangle2 (geom/tessellate _))
              triangle (random-weighted-by geom/area triangles)]
          ;; FIXME: triangle random point is barycentric and not uniform
          ;; https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
          (geom/random-point-inside triangle))))

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
        shape2 (rect/rect (* 0.6 w) (* 0.15 h) (* 0.3 w) (* 0.4 h))
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
