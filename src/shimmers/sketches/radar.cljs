(ns shimmers.sketches.radar
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as tg]
            [thi.ng.math.core :as tm]))

(def green [0 230 0 200])

(defn setup []
  (let [hw (/ (q/width) 2)
        hh (/ (q/height) 2)
        radius (* 0.45 (q/height))]
    (apply q/stroke green)
    (apply q/fill green)
    (q/ellipse-mode :radius)
    (q/ellipse hw hh radius radius)
    {:theta 0.0
     :center (v/vec2 hw hh)
     :radius radius
     :point (v/vec2 0 60)}))

(defn update-state [state]
  (-> state
      (update :theta + 0.012)
      (update :point v/add (v/vec2 0.15 0.02))
      (update :point (fn [pos] (v/wrap2d pos (q/width) (q/height))))))

(defn draw [{:keys [theta center radius point]}]
  (q/background 0 3)
  (apply q/stroke green)
  (apply q/translate center)
  (let [tpoint (tm/- point center)
        delta (- (tg/heading tpoint) (mod theta (* 2 Math/PI)))]
    (when (and (< (tg/dist (v/vec2 0 0) tpoint) radius)
               (< -0.1 delta 0.001))
      (println [(tg/heading tpoint) (mod theta (* 2 Math/PI)) delta tpoint])
      (apply q/point tpoint)))
  (q/stroke-weight 2)
  (q/point 0 0)
  (q/stroke-weight 0.8)
  (q/line 0 0 (* radius (q/cos theta)) (* radius (q/sin theta))))

(defn ^:export run-sketch []
  (q/defsketch radar
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
