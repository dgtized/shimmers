(ns shimmers.sketches.offsetting-circles
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]))

;; A variation on https://twitter.com/incre_ment/status/1482584406933979136
(defn setup []
  (set! (.-disableFriendlyErrors js/p5) true)
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.01))

;; TODO: consider using video as input instead of noise?
;; also any means to up the resolution
(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke 1.0)
  (q/fill 0.1 0.3)
  ;; (q/no-stroke)
  (let [scale 10]
    (doseq [a (range 0 800 scale)]
      (doseq [b (range 0 600 scale)]
        (let [n (q/noise (* a 0.01) (* b 0.01) t)
              x (+ a (* n (Math/tan (* eq/TAU (+ t n)))))]
          (cq/circle x (+ b (- x a)) scale))))))

(sketch/defquil offsetting-circles
  :created-at "2022-01-16"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
