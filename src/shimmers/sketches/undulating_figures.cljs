(ns shimmers.sketches.undulating-figures
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/noise-detail 6 0.4)
  (q/frame-rate 30)
  (q/color-mode :hsl 1.0)
  {})

;; How to make interlace overlap so one figure is drawn with over draw from the
;; left, and the other from the right, so all the figures are in the overlap?
(defn draw [_]
  (q/background 1.0)
  (q/no-stroke)
  (q/no-fill)
  (let [[ma mb] [(q/noise (/ (q/frame-count) 160) 50 50)
                 (q/noise (/ (q/frame-count) 220) 90 100)]
        [ma mb] (if (<= ma mb) [ma mb] [mb ma])]
    (loop [y 0]
      (when (<= y 1.0)
        (let [a (max ma (rand))
              b (max mb (rand))
              thickness (* 0.01 (rand))
              padding (* 0.015 (rand))]
          (q/stroke 0.95 0.5 0.5 1.0)
          (q/rect (cq/rel-w (- 0.9 (* 0.4
                                      (q/noise (/ (q/frame-count) 200) y))))
                  (cq/rel-h y)
                  (cq/rel-w (- b))
                  (cq/rel-h (* 0.7 thickness)))
          (q/stroke 0.55 0.5 0.5 1.0)
          (q/rect (cq/rel-w (+ 0.1 (* 0.4
                                      (q/noise (/ (q/frame-count) 180) y))))
                  (cq/rel-h y)
                  (cq/rel-w a)
                  (cq/rel-h (* 0.3 thickness)))
          (recur (+ y thickness padding)))))))

(sketch/defquil undulating-figures
  :created-at "2021-06-05"
  :size [1024 768]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
