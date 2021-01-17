(ns shimmers.sketches.radar
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as tg]
            [thi.ng.math.core :as tm]))

(defrecord Particle [position velocity])

(defn step [{:keys [position velocity] :as particle}]
  (-> particle
      (assoc :position (v/add position velocity))
      (update :position (fn [pos] (v/wrap2d pos (q/width) (q/height))))))

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
     :particles [(->Particle (v/vec2 0 60) (v/vec2 0.15 0.02))
                 (->Particle (v/vec2 (* 2 hw) 60) (v/vec2 -0.2 0.02))
                 (->Particle (v/vec2 hw 0) (v/vec2 -0.02 0.1))
                 (->Particle (v/vec2 120 0) (v/vec2 -0.05 0.15))]}))

(defn update-state [state]
  (-> state
      (update :theta + (/ (* 2 Math/PI) (* 60 15)))
      (update :particles (partial map step))))

(defn draw-point-hit [theta radius center point]
  (let [tpoint (tm/- point center)
        heading (tg/heading tpoint)
        mtheta (mod theta (* 2 Math/PI))
        delta (- heading mtheta)]
    (when (and (< (tg/dist (v/vec2 0 0) tpoint) radius)
               (< -0.1 delta 0.001))
      ;; (println [heading mtheta delta tpoint])
      (apply q/point tpoint))))

(defn draw [{:keys [theta center radius particles]}]
  (when (= 0 (mod (q/frame-count) 6))
    (q/background 0 8))
  (apply q/stroke (green 255))
  (apply q/translate center)
  (q/stroke-weight 3)
  (doseq [{:keys [position]} particles]
    (draw-point-hit theta radius center position))
  (apply q/stroke (green 192))
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
