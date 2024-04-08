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
  (q/frame-rate 24)
  {:alpha 0.0
   :points 512})

(defn update-state [state]
  (update state :alpha (fn [a] (mod (+ a 1.0) 10.0))))

(defn draw [{:keys [points alpha]}]
  (q/background 1.0)
  (let [center (cq/rel-vec 0.5 0.5)
        radius (* 0.45 (min (q/height) (q/width)))
        exterior (min (int (tm/roundto (* alpha (math/sqrt points)) 0.1)) points)
        interior (- points exterior)
        k-theta (* math/PI (- 3 (math/sqrt 5)))]
    (doseq [[r theta] (mapv (fn [i] [(if (< i interior) (/ (float i) (inc interior)) 1.0)
                                    (* i k-theta)])
                            (range points))]
      (cq/circle (v/+polar center (* r radius) theta)
                 2.0))))

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
