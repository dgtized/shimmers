(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.probability :as p]
            [shimmers.math.geometry :refer [rotate-around-centroid]]
            [thi.ng.geom.circle :as tc]
            [thi.ng.geom.core :as geom]
            thi.ng.geom.polygon
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.types :refer [Polygon2]]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; http://extremelearning.com.au/evenly-distributing-points-in-a-triangle/
;; https://stackoverflow.com/questions/47410054/generate-random-locations-within-a-triangular-domain/47418580#47418580
(defn random-point-in-triangle2 [{:keys [points]}]
  (let [[s t] (sort [(rand) (rand)])
        weighting (gv/vec3 s (- t s) (- 1 t))]
    (gv/vec2 (tm/dot (apply gv/vec3 (map :x points)) weighting)
             (tm/dot (apply gv/vec3 (map :y points)) weighting))))

;; https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
(extend-type Polygon2
  geom/ISample
  (random-point-inside
    [_] (->> (geom/tessellate _)
             (map gt/triangle2)
             (p/weighted-by geom/area)
             random-point-in-triangle2)))

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
        shape2 (rotate-around-centroid (rect/rect (* 0.55 w) (* 0.2 h) (* 0.3 w) (* 0.4 h))
                                       0.2)
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
