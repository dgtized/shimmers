(ns shimmers.sketches.reagent-quil-component
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (cq/circle (v/polar (cq/rel-h 0.4) t) 20))

(defn page []
  [:div
   [:p "before"]
   (sketch/component
    :size [800 300]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p "between"]
   (sketch/component
    :size [800 300]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p "after"]])

(sketch/definition reagent-quil-component
  {:created-at "2022-03-29"
   :type :quil
   :tags #{}}
  (ctrl/mount page "sketch-host"))
