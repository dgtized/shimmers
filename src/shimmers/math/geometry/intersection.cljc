(ns shimmers.math.geometry.intersection
  (:require [thi.ng.math.core :as tm]))

;; https://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
;; https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection
(defn circle-segment-intersection
  [{:keys [c r]} {[p q] :points}]
  (let [d (tm/- q p)
        f (tm/- p c)
        a (tm/dot d d)
        b (* 2 (tm/dot f d))
        c (- (tm/dot f f) (* r r))
        discriminant (- (* b b) (* 4 a c))]
    (when (>= discriminant 0)
      (let [root-disc (Math/sqrt discriminant)
            reciprocal (/ 1 (* 2 a))
            t1 (* (- (- b) root-disc) reciprocal)
            t2 (* (+ (- b) root-disc) reciprocal)]
        [t1 t2 (tm/+ p (tm/* d t1)) (tm/+ p (tm/* d t2))]))))
