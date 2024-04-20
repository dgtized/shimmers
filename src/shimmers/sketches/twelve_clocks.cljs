(ns shimmers.sketches.twelve-clocks
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw-hand [step angle radius time]
  (let [a (v/+polar (cq/rel-vec 0.5 0.5)
                    radius
                    (+ angle (* 0.1 (eq/unit-sin (+ (* 0.25 step) (* 0.075 time))))))
        b (v/+polar a (/ radius 6)
                    (+ angle time
                       (* 0.1 step)
                       (math/sin (- angle (* 0.1 time)))))
        c (v/+polar b (/ radius 9)
                    (- angle (* 1.5 time)
                       (* 0.2 step (eq/unit-sin (+ angle time)))
                       (Math/sin (+ angle (* 0.3 time)))))
        d (v/+polar c (/ radius 21)
                    (+ angle (* 2.5 time)
                       (* 0.3 step (eq/unit-sin time))
                       (Math/sin (+ angle (* 0.5 time)))))]
    (q/stroke-weight (+ 1.5 (math/sin (+ angle (* 0.1 time)))))
    (q/line a b)
    (q/line b c)
    (q/line c d)))

(defn draw [_state]
  (q/background 1.0)
  (let [time (/ (q/millis) 1000.0)
        radius (cq/rel-h 0.35)
        dist (* 20 (Math/sin (* 0.2 time)))]
    (doseq [step (range 7)
            hand (range 12)]
      (draw-hand step
                 (* eq/TAU (+ (/ hand 12.0) (math/sin (* 0.025 time))))
                 (- radius (* dist step)) time))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition twelve-clocks
  {:created-at "2024-04-19"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
