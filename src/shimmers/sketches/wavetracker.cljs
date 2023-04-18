(ns shimmers.sketches.wavetracker
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (let [opacity (+ 0.1 (* 0.9 (eq/unit-cos (* tm/THREE_HALVES_PI t))))]
    (q/background 1.0 opacity)
    (q/no-stroke)
    (q/fill 0.0 1.0))
  (q/translate (cq/rel-vec 0.0 0.5))
  (let [samples 75
        rate (* 30 Math/PI (+ 0.5 (eq/unit-sin (* 0.9 tm/QUARTER_PI t))))
        amplitude (* (cq/rel-h 0.4) (+ 0.2 (* 0.8 (eq/unit-sin (* 0.66 t)))))
        jitter (tm/smoothstep* 0.33 0.80 (eq/unit-sin (* 0.37 t)))]
    (dotimes [j 10]
      (let [time-factor (+ t (* 0.2 j))]
        (dotimes [i samples]
          (let [x (* (mod (/ (float i) samples) 1.0) (q/width))
                y (* (Math/cos (+ time-factor (/ x rate))))
                scale (+ 0.25 (* 0.75 (eq/unit-sin (+ (* 2.5 t) (* j 0.5)))))]
            (cq/circle (tm/+ (gv/vec2 x (* amplitude y))
                             (dr/jitter (* 8 jitter (- 1.0 scale))))
                       (abs (* scale 4.0)))))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition wavetracker
  {:created-at "2023-04-17"
   :type :quil
   :tags #{}}
  (ctrl/mount page "sketch-host"))
