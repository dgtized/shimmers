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
        radius 0.2
        w1 (* eq/TAU (+ 0.1 (* 0.35 (eq/unit-sin t))))
        a (* 0.5 (math/sin (+ w1 (* 0.33 t))))
        base (* eq/TAU (math/sin (+ a (* 0.08 t))))]
    (doseq [s (range (- w1) w1 0.05)]
      (let [w2 (* 0.05 (math/sin (+ w1 (math/sin (+ (* 0.66 t) (* 2.81 s) base)))))
            v1 (* 0.15 (math/sin (+ s (* 0.2 t) base)))
            b (* 0.5 (math/sin (+ s a (* 0.25 t))))]
        (doseq [v (range (- w2) w2 0.02)]
          (let [vw (+ 0.6 (eq/unit-cos (+ (* 0.21 eq/TAU s) (* 0.35 t))))
                p1 (-> center
                       (v/+polar (cq/rel-h (+ radius v)) (+ (* 1.07 s) (* 0.66 t) a))
                       (v/+polar (cq/rel-h (* vw (+ v v1))) (+ (* 2.19 s) (* 0.1 t) b)))
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
