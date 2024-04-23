(ns shimmers.sketches.slither
  (:require
   [clojure.math :as math]
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
  {:t 0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (q/no-fill)
  (q/background 1.0 0.3)
  (q/translate (cq/rel-vec 0.5 0.5))
  (let [h (* (cq/rel-h 0.49) (+ 0.8 (* 0.2 (+ (* 0.7 (eq/unit-cos (+ 0.1 (* 0.4 t))))
                                              (* 0.3 (eq/unit-cos (* 1.7 t)))))))
        b (* (cq/rel-h 0.05) (+ 0.1 (eq/unit-sin (* 0.65 t))))]
    (doseq [v (tm/norm-range 23)]
      (q/begin-shape)
      (apply q/curve-vertex (v/polar b (* eq/TAU v)))
      (apply q/curve-vertex (v/polar (+ b (* h 0.05)) (* eq/TAU (+ v (* 0.075 (math/sin (* 0.33 t)))))))
      (apply q/curve-vertex (v/polar (* h (+ 0.5 (* 0.125 (math/cos (* 0.9 t)))))
                                     (* eq/TAU (+ v (* 0.09 (math/sin (* 1.33 t)))))))
      (apply q/curve-vertex (v/polar (* h 0.95) (* eq/TAU (+ v (* 0.075 (math/sin (* 0.25 t)))))))
      (apply q/curve-vertex (v/polar (* h 1.0) (* eq/TAU v)))
      (q/end-shape))))

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
