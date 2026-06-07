(ns shimmers.sketches.warped-grid
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {})

(defn update-state [state]
  state)

(defn f [i j t]
  (gv/vec2
   (+ i (* 0.05 (math/cos (* j (math/tan (+ (* math/PI i) t))))))
   (+ j (* 0.05 (math/sin (* i (math/sin (+ (* math/PI j) t))))))))

(defn draw [state]
  (q/background 1.0)
  (q/stroke-weight 0.9)
  (let [t (/ (q/millis) 10000.0)]
    (doseq [i (tm/norm-range 80)
            j (tm/norm-range 60)]
      (cq/circle (cq/rel-vec (f i j t))
                 1.0)))
  state)

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
