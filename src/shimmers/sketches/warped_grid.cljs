(ns shimmers.sketches.warped-grid
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn stime []
  (/ (q/millis) 10000.0))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:v 1.0})

(defn update-state [state]
  (let [t (* 0.4 (stime))]
    (if (dr/chance (* 0.1 (eq/unit-sin (+ t (eq/unit-sin (* 1.33 t))))))
      (assoc state :v (dr/rand-nth [1.0 1.33 1.66 2.0]))
      state)))

(defn f [i j t v]
  (gv/vec2
   (+ i (* 0.05 (math/cos
                 (* math/PI (+ (math/sin (* math/PI t)) j)
                    (math/tan (+ (* math/PI i) t))))))
   (+ j (* 0.05 (math/sin (* math/PI i (math/sin (+ (* math/PI j)
                                              (* t v)))))))))

(defn draw [{:keys [v]}]
  (q/background 1.0)
  (q/stroke-weight 0.9)
  (let [t (stime)]
    (doseq [i (tm/norm-range 80)
            j (tm/norm-range 60)]
      (cq/circle (cq/rel-vec (f (- (* 1.1 i) 0.05) (- (* 1.1 j) 0.05) t v))
                 1.0))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition warped-grid
  {:created-at "2026-06-06"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
