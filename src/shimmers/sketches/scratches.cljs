(ns shimmers.sketches.scratches
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.math.core :as tm]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/noise-detail 6 0.4)
  (q/frame-rate 30)
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0 1.0)
  (loop [y 0]
    (when (<= y 1.0)
      (let [a (rand)
            b (rand)
            thickness (* 0.01 (rand))
            padding (* 0.01 (rand))]
        (q/rect (cq/rel-w a) (cq/rel-h y)
                (cq/rel-w (* 0.8
                             (q/noise (/ (q/frame-count) 100) y)))
                (cq/rel-h thickness))
        (q/rect (cq/rel-w b) (cq/rel-h y)
                (cq/rel-w (* 0.4
                             (q/noise (/ (q/frame-count) 100) y)))
                (cq/rel-h (* 0.5 thickness)))
        (recur (+ y thickness padding))))))

(defn ^:export run-sketch []
  ;; 20210605
  (q/defsketch scratches
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
