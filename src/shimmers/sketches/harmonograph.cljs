(ns shimmers.sketches.harmonograph
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn dampen [lambda t]
  (Math/exp (* (- lambda) t)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 1))

(defn modular-stroke [t]
  (q/stroke-weight (+ 1.2 (* 0.6 (Math/sin (* 2 t))))))

(defn skip-draw [k t]
  (when (> (Math/sin (+ (* (/ 7 5) t) (* 3 (Math/sin (* (/ 1 5) t))))) 0)
    (modular-stroke t)
    (apply q/point (v/polar (* 0.3 (q/height) k) (* (/ 1 6) t)))))

(defn draw [{:keys [t]}]
  (dotimes [i 1000]
    (let [t (+ (* 4.0 t) (/ i 200))
          k (dampen 0.09 (* 0.01 t))]
      (q/with-translation
        [(tm/+ (cq/rel-vec 0.5 0.5)
               (v/polar (* 0.15 (q/height) k) (* (/ 1 4) t)))]
        (skip-draw k t)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition harmonograph
  {:created-at "2024-01-22"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
