(ns shimmers.sketches.descending-lines
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:dt 0.0
   :t (/ (q/millis) 1000.0)})

(defn update-state [{:keys [t] :as state}]
  (let [dt (- (/ (q/millis) 1000.0) t)]
    (-> state
        (assoc :dt dt)
        (update :t + dt))))

(defn draw [{:keys [t dt]}]
  (q/stroke (/ (mod t 6.0) 6.0) 0.5 0.5)
  (let [x (cq/rel-w (/ (mod t 4.0) 4.0))]
    (q/line x 0 x (q/height))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition descending-lines
  {:created-at "2024-01-10"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
