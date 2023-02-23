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

(defn draw-samples [n pos-f]
  (dotimes [_ n] (cq/circle (pos-f) 1.0)))

(defn draw [_]
  (q/color-mode :hsl 1.0)
  (q/fill 0)
  (q/no-stroke)
  (let [center (cq/rel-vec 0.15 0.4)]
    (doseq [radius (range 0.05 1.0 0.08)]
      (draw-samples
       (* radius 12000)
       (fn []
         (let [r (* (cq/rel-h radius) (Math/sqrt (dr/gaussian 0.9 0.08)))
               theta (dr/gaussian 0.0 0.5)]
           (v/+polar center r theta))))))
  (let [center (cq/rel-vec 0.8 0.55)]
    (doseq [radius (range 0.05 0.9 0.05)]
      (draw-samples
       8000
       (fn []
         (let [r (* (cq/rel-h radius) (Math/sqrt (dr/gaussian 0.9 0.15)))
               theta (dr/gaussian (* 0.45 eq/TAU) 0.8)]
           (v/+polar center r theta))))))
  (q/fill 0.0 0.7 0.35)
  (let [center (cq/rel-vec 0.55 0.85)]
    (doseq [radius (range 0.05 0.8 0.1)]
      (draw-samples
       (* radius 16000)
       (fn []
         (let [r (* (cq/rel-h radius) (Math/sqrt (dr/gaussian 0.9 0.1)))
               theta (dr/gaussian (* 0.75 eq/TAU) (* 0.35 (- 1.2 radius)))]
           (v/+polar center r theta))))))
  (q/no-loop))

(sketch/defquil pixel-rings
  :created-at "2023-02-23"
  :tags #{}
  :size [900 600]
  :draw draw
  :middleware [m/fun-mode framerate/mode])
