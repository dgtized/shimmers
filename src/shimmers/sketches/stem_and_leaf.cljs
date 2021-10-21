(ns shimmers.sketches.stem-and-leaf
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as g]
            [thi.ng.math.core :as tm]))

;; The intended concept is to plot out bezier curves from a source circle or
;; stem, and then add circles that are tangent to the curve, and then randomly
;; extend new bezier curves out from the circles generated before. Presumably
;; those curves should not intersect, and neither should the circles so there is
;; some circle packing as well. It's likely there is going to be a lot of S
;; curves with tangents on one side mirrored on the "outside". Also, each circle
;; may need to have a "direction" of rotation that curves emit from so they pair
;; up.

;; Right now it's just playing with straight lines from tangent points, but the
;; circles are arranged somewhat analagous to perspective lines to focal points,
;; so alternatively could make a different sketch around rotating perspectives.

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [(gc/circle (cq/rel-pos 0.5 0.5) (cq/rel-h 0.1))
             (assoc (gc/circle (cq/rel-pos 0.2 0.5) (cq/rel-h 0.15)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.8 0.5) (cq/rel-h 0.05)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.5 0.2) (cq/rel-h 0.15)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.5 0.8) (cq/rel-h 0.05)) :parent 0)]})

(defn update-state [state]
  state)

(defn tangent-lines [c1 c2]
  (let [{:keys [p r]} c1
        {p' :p r' :r} c2
        angle (+ (* 0.5 Math/PI) (g/heading (tm/- p p')))]
    (q/line (tm/+ p (v/polar r angle)) (tm/+ p' (v/polar r' angle)))
    (q/line (tm/- p (v/polar r angle)) (tm/- p' (v/polar r' angle)))))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 1.2)
  (doseq [{:keys [p r]} circles]
    (cq/circle p r))
  (doseq [{:keys [parent] :as c1} circles
          :when parent
          :let [c2 (nth circles parent)]]
    (tangent-lines c1 c2)

    #_(cq/draw-curve-path [(tm/+ p (v/polar r (* 1.5 Math/PI)))
                           (tm/+ p (v/polar r (* 1.6 Math/PI)))
                           (tm/+ p (v/polar r (* 1.7 Math/PI)))
                           (tm/+ p' (tm/* (tm/- p p') 0.5)) ;; midpoint
                           (tm/+ p' (v/polar r' (* 0.50 Math/PI)))
                           (tm/+ p' (v/polar r' (* 0.33 Math/PI)))])
    )
  ;; Draw all the sibling tangents?
  (q/stroke-weight 0.5)
  (doseq [a [1 2]
          b [3 4]]
    (tangent-lines (nth circles a) (nth circles b))))

(sketch/defquil stem-and-leaf
  :created-at "2021-07-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
