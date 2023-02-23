(ns shimmers.sketches.pixel-rings
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

(defn draw [_]
  (q/color-mode :hsl 1.0)
  (q/fill 0)
  (q/no-stroke)
  (let [center (cq/rel-vec 0.15 0.4)]
    (doseq [radius (range 0.05 1.0 0.08)]
      (dotimes [_ (* radius 12000)]
        (let [r (* (cq/rel-h radius) (Math/sqrt (dr/gaussian 0.9 0.08)))
              theta (dr/gaussian 0.0 0.5)]
          (cq/circle (v/+polar center r theta) 1.0)))))
  (let [center (cq/rel-vec 0.8 0.55)]
    (doseq [radius (range 0.05 0.9 0.05)]
      (dotimes [_ 8000]
        (let [r (* (cq/rel-h radius) (Math/sqrt (dr/gaussian 0.9 0.15)))
              theta (dr/gaussian (* 0.45 eq/TAU) 0.8)]
          (cq/circle (v/+polar center r theta) 1.0)))))
  (q/fill 0.0 0.7 0.35)
  (let [center (cq/rel-vec 0.55 0.85)]
    (doseq [radius (range 0.05 0.8 0.1)]
      (dotimes [_ (* radius 16000)]
        (let [r (* (cq/rel-h radius) (Math/sqrt (dr/gaussian 0.9 0.1)))
              theta (dr/gaussian (* 0.75 eq/TAU) (* 0.35 (- 1.2 radius)))]
          (cq/circle (v/+polar center r theta) 1.0)))))
  (q/no-loop))

(sketch/defquil pixel-rings
  :created-at "2023-02-23"
  :tags #{}
  :size [900 600]
  :draw draw
  :middleware [m/fun-mode framerate/mode])
