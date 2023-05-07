(ns shimmers.sketches.offsetting-arcs
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]))

;; A variation on https://twitter.com/incre_ment/status/1482584406933979136
(defn setup []
  (set! (.-disableFriendlyErrors js/p5) true)
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.005))

;; TODO: consider using video as input instead of noise?
;; also any means to up the resolution
(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-stroke)
  (q/fill 0.0 0.4)
  ;; (q/no-stroke)
  (let [scale 10]
    (doseq [a (range 0 800 scale)]
      (doseq [b (range 0 600 scale)]
        (let [n (q/noise (* a 0.01) (* b 0.01) t)
              x (+ a (* n (Math/tan (* eq/TAU (+ t n)))))]
          (q/arc x (+ b (- x a)) scale scale (* eq/TAU (Math/sin t)) (* eq/TAU n)))))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition offsetting-arcs
  {:created-at "2022-01-16"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
