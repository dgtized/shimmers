(ns shimmers.sketches.wavetracker
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.equations :as eq]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/fill 0.0)
  ;; (q/translate (cq/rel-vec 0.5 0.5))
  (let [samples 150]
    (dotimes [j 10]
      (dotimes [i samples]
        (let [x (* (mod (/ (float i) samples) 1.0) (q/width))
              y (+ (cq/rel-h 0.5)
                   (* (cq/rel-h 0.4)
                      (Math/cos (+ t (* 0.2 j) (/ x (* 30 Math/PI (+ 0.5 (eq/unit-sin (* 2 t)))))))))]
          (cq/circle (gv/vec2 x y) 3.0))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition wavetracker
  {:created-at "2023-04-17"
   :type :quil
   :tags #{}}
  (ctrl/mount page "sketch-host"))
