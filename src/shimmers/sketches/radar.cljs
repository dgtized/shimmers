(ns shimmers.sketches.radar
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as tg]
            [thi.ng.math.core :as tm]))

(defn green [x] [0 230 0 x])

(defn setup []
  (let [hw (/ (q/width) 2)
        hh (/ (q/height) 2)
        radius (* 0.45 (q/height))]
    (apply q/stroke (green 255))
    (apply q/fill (green 32))
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
  (when (= 0 (mod (q/frame-count) 8))
    (q/background 0 12))
  (apply q/stroke (green 255))
  (apply q/translate center)
  (q/stroke-weight 2)
  (let [tpoint (tm/- point center)
        delta (- (tg/heading tpoint) (mod theta (* 2 Math/PI)))]
    (when (and (< (tg/dist (v/vec2 0 0) tpoint) radius)
               (< -0.1 delta 0.001))
      (println [(tg/heading tpoint) (mod theta (* 2 Math/PI)) delta tpoint])
      (apply q/point tpoint)))
  (q/point 0 0)
  (apply q/stroke (green 255))
  (q/stroke-weight 1)
  (q/line 0 0 (* radius (q/cos theta)) (* radius (q/sin theta))))

(defn ^:export run-sketch []
  (q/defsketch radar
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
