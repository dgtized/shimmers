(ns shimmers.sketches.bidirectional-growth
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

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 4.0)
  (let [t (* (q/millis) 0.0005)
        center (cq/rel-vec 0.5 0.5)
        radius 0.25
        w1 (* eq/TAU (+ 0.1 (* 0.35 (eq/unit-sin t))))]
    (doseq [s (range (- w1) w1 0.05)]
      (let [w2 (* 0.05 (math/sin (+ w1 (math/sin (+ (* 0.66 t) (* 2 s))))))
            v1 (* 0.15 (math/sin (+ s (* 0.2 t))))]
        (doseq [v (range (- w2) w2 0.025)]
          (let [p1 (-> center
                       (v/+polar (cq/rel-h (+ radius v v1)) (+ s (* 0.66 t)))
                       (v/+polar (cq/rel-h (+ v v1)) (+ (* 2.5 s) (* 0.1 t))))
                [x y] p1]
            (q/point x y)))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition bidirectional-growth
  {:created-at "2026-02-23"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
