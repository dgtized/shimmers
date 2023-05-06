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
  (q/ellipse-mode :radius)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (let [opacity (+ 0.1 (* 0.9 (tm/smoothstep* 0.2 0.7 (eq/unit-cos (+ 0.7 (* 1.47 t))))))]
    (q/background 1.0 opacity)
    (q/no-stroke)
    (q/fill 0.0 1.0))
  (let [samples 75
        rate (* 30 Math/PI (+ 0.5 (eq/unit-sin (- (* 0.8 tm/QUARTER_PI t) 0.05))))
        amplitude (* (cq/rel-h 0.4) (+ 0.2 (* 0.8 (eq/unit-sin (- (* 0.53 t) (/ 1 5))))))
        jitter (tm/smoothstep* 0.39 0.9 (eq/unit-sin (* 0.31 t)))
        max-scale (cq/rel-h 0.004)
        max-jitter (* 2 max-scale)
        x-slide (let [s (Math/cos (+ 0.25 (* 0.12 t)))]
                  (* 3 (tm/sign s) (tm/smoothstep* 0.35 0.75 (abs s))))
        width (+ (* 0.6 (eq/unit-sin (* 0.1 (* 0.6 t))))
                 (* 0.3 (eq/unit-sin (+ 0.2 (* 1.1 t))))
                 (* 0.1 (eq/unit-sin (+ 0.3 (* 1.4 t)))))
        wobble (tm/smoothstep* 0.65 0.95 (eq/unit-sin (+ (/ 1 7) (* (/ 1 6) t))))
        wibble (+ 1.3 (* 0.4 (Math/sin (+ (/ 2 7) (* 1.41 t)))))]
    (dotimes [j 9]
      (let [time-factor (+ t (* 0.25 (inc j) width))]
        (dotimes [i samples]
          (let [x-norm (+ (float i) (* x-slide (eq/unit-sin (+ (* 1.66 t) (/ j 18)))))
                x (* (mod (/ x-norm samples) 1.0) (q/width))
                y (+ (* (- 1.0 (* 0.25 wobble)) (Math/cos (+ time-factor (/ x rate))))
                     (* 0.25 wobble
                        (Math/cos (+ 1.1 time-factor (/ (* wibble x) rate)))))
                scale (+ 0.25 (* 0.75 (eq/unit-sin (+ (* 2.13 t) (* j 0.5)))))]
            (cq/circle (tm/+ (gv/vec2 x (+ (* amplitude y) (cq/rel-h 0.5)))
                             (dr/jitter (* max-jitter jitter (- 1.0 scale))))
                       (abs (* scale max-scale)))))))))

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
  (ctrl/mount page))
