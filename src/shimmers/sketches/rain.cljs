(ns shimmers.sketches.precipitation
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn rain [noise]
  (let [x0 (q/random (- 140) (+ 140 (q/width)))
        angle (q/radians (+ (* 3 (q/random-gaussian))
                            (* 110 (- 1 noise))))
        x1 (+ x0 (* 200 (q/cos angle)))
        s0 (- (rand) 0.2)
        s1 (+ s0 (q/random noise))]
    (q/stroke-weight (q/random 0.1 noise))
    (q/line (q/lerp x0 x1 s0) (q/lerp 0 (q/height) s0)
            (q/lerp x0 x1 s1) (q/lerp 0 (q/height) s1))))

(defn draw [_]
  (q/background 0 10)
  (q/stroke 255 128)
  (let [noise (q/noise 0 (/ (q/frame-count) 1000))]
    (q/print-every-n-millisec 500 noise)
    (dotimes [i (q/round (q/random (* 8 noise)))]
      (rain noise))))

(defn ^:export run-sketch []
  (q/defsketch precipitation
    :host "quil-host"
    :size [600 400]
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
