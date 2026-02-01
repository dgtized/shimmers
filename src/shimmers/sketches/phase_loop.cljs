(ns shimmers.sketches.phase-loop
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]
   [clojure.math :as math]))

(defn setup []
  (q/color-mode :hsl 1.0))

(defn update-state [state]
  state)

(defn f1 [x t]
  (eq/unit-sin (* eq/TAU (+ x (* 0.1 t)
                       (* 0.2 (math/cos (+ x t)))))))

(defn f2 [x t]
  (eq/unit-cos (* eq/TAU (+ x (* 0.3 t)
                       (math/sin (+ x t))))))

(defn draw [_]
  (q/background 1.0)
  (let [t (* 0.001 (q/millis))]
    (doseq [x (tm/norm-range 90)]
      (q/line (cq/rel-vec x (f1 x t))
              (cq/rel-vec x (f2 x t))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition phase-loop
  {:created-at "2026-01-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
