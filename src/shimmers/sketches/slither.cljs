(ns shimmers.sketches.slither
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.quil :as cq]
   [shimmers.math.vector :as v]
   [shimmers.math.equations :as eq]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (q/no-fill)
  (q/background 1.0 0.25)
  (q/translate (cq/rel-vec 0.5 0.5))
  (doseq [v (tm/norm-range 23)]
    (q/begin-shape)
    (apply q/curve-vertex (v/polar (cq/rel-h 0.01) (* eq/TAU v)))
    (apply q/curve-vertex (v/polar (cq/rel-h 0.05) (* eq/TAU v)))
    (apply q/curve-vertex (v/polar (cq/rel-h (+ 0.25 (* 0.15 (Math/cos t))))
                                   (* eq/TAU (+ v (* 0.1 (Math/sin (* 1.5 t)))))))
    (apply q/curve-vertex (v/polar (cq/rel-h 0.45) (* eq/TAU v)))
    (apply q/curve-vertex (v/polar (cq/rel-h 0.49) (* eq/TAU v)))
    (q/end-shape)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition slither
  {:created-at "2023-07-29"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
