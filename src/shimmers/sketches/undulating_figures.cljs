(ns shimmers.sketches.undulating-figures
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/noise-detail 6 0.4)
  (q/frame-rate 30)
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

;; How to make interlace overlap so one figure is drawn with over draw from the
;; left, and the other from the right, so all the figures are in the overlap?
(defn draw [state]
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0 1.0)
  (let [[ma mb] (sort [(q/noise (/ (q/frame-count) 100) 50 50)
                       (q/noise (/ (q/frame-count) 100) 90 100)])
        [ma mb] (if (<= ma mb) [ma mb] [mb ma])]
    (loop [y 0]
      (when (<= y 1.0)
        (let [a (min ma (rand))
              b (max mb (rand))
              thickness (* 0.01 (rand))
              padding (* 0.01 (rand))]
          (q/rect (cq/rel-w (+ 0.2 (* 0.8
                                      (q/noise (/ (q/frame-count) 200) y))))
                  (cq/rel-h y)
                  (cq/rel-w (- a))
                  (cq/rel-h thickness))
          (q/rect (cq/rel-w (* 0.5
                               (q/noise (/ (q/frame-count) 180) y)))
                  (cq/rel-h y)
                  (cq/rel-w b)
                  (cq/rel-h (* 0.5 thickness)))
          (recur (+ y thickness padding)))))))

(defn ^:export run-sketch []
  ;; 20210605
  (q/defsketch undulating-figures
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
