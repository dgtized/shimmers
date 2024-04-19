(ns shimmers.sketches.twelve-clocks
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw-hand [angle radius time]
  (let [base (v/+polar (cq/rel-vec 0.5 0.5) radius angle)
        q (v/+polar base (/ radius 6) (+ angle time))
        p (v/+polar q (/ radius 9) (- angle (* 3 time) (Math/sin (+ angle (* 0.5 time)))))]
    (q/line base q)
    (q/line q p)))

(defn draw [state]
  (q/background 1.0)
  (let [time (/ (q/millis) 1000.0)
        radius (cq/rel-h 0.35)
        dist (* 20 (Math/sin (* 0.2 time)))]
    (doseq [step (range (+ 5 (Math/floor (* 4.0 (Math/sin (* tm/PHI time))))))
            hand (range 12)]
      (draw-hand (* eq/TAU (/ hand 12.0)) (- radius (* dist step)) time))))

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
