(ns shimmers.sketches.sunflower
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [thi.ng.math.core :as tm]
   [shimmers.math.vector :as v]
   [clojure.math :as math]))

;; https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle
(defn setup []
  (q/color-mode :hsl 1.0)
  {:alpha 0.0
   :points 768})

(defn update-state [state]
  (let [t (/ (q/millis) 1000.0)]
    (assoc state :alpha (+ 6.0 (* 6.0 (math/sin (* 0.25 t))))
           :t t)))

(defn draw [{:keys [points alpha t]}]
  (q/background 1.0)
  (let [center (cq/rel-vec 0.5 0.5)
        radius (* 0.45 (min (q/height) (q/width)))
        exterior (min (int (tm/roundto (* alpha (math/sqrt points)) 0.1)) points)
        interior (- points exterior)
        k-theta (* math/PI (- 3 (math/sqrt 5)))]
    (doseq [[r theta] (mapv (fn [i] [(if (< i interior) (/ (float i) (inc interior)) 1.0)
                                    (+ (* 0.05 t) (* i k-theta))])
                            (range points))]
      (cq/circle (v/+polar center (* r radius) theta)
                 (+ 2.5 (* 1.5 (math/sin (+ t theta))))))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition sunflower
  {:created-at "2024-04-08"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
