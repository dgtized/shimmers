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
    {:theta 0.0
     :center (v/vec2 hw hh)
     :radius radius
     :particles [(->Particle (v/vec2 0 60) (v/vec2 0.15 0.02))
                 (->Particle (v/vec2 (* 2 hw) 60) (v/vec2 -0.2 0.02))
                 (->Particle (v/vec2 hw 0) (v/vec2 -0.02 0.1))
                 (->Particle (v/vec2 120 0) (v/vec2 -0.05 0.15))]
     :contacts (q/create-graphics (q/width) (q/height))
     :sweep (q/create-graphics (q/width) (q/height))}))

(defn update-state [state]
  (-> state
      (update :theta + (/ (* 2 Math/PI) (* 40 15)))
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

(defn draw
  [{:keys [contacts sweep particles
           theta center radius]}]
  (q/with-graphics contacts
    (q/push-matrix)
    (apply q/translate center)
    (when (= 0 (mod (q/frame-count) 24))
      (q/background 0 6))
    (apply q/stroke (green 255))
    (q/stroke-weight 2.5)
    (doseq [{:keys [position]} particles]
      (draw-point-hit theta radius center position))
    (q/pop-matrix))
  (q/with-graphics sweep
    (q/push-matrix)
    (apply q/translate center)
    (when (= 0 (mod (q/frame-count) 10))
      (q/background 0 8))
    (apply q/stroke (green 255))
    (q/stroke-weight 2)
    (q/line 0 0 (* radius (q/cos theta)) (* radius (q/sin theta)))
    (q/pop-matrix))

  ;; (q/background 0)
  (q/image contacts 0 0)
  (q/tint 255 200)
  (q/image sweep 0 0)
  )

(defn ^:export run-sketch []
  (q/defsketch radar
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
