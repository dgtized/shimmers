(ns shimmers.sketches.two-bubbles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn setup []
  (q/frame-rate 60)
  {:direction 0.005
   :r1 100
   :r2 100})

(defn migrate-volume [{:keys [direction r1 r2] :as state}]
  (let [rN (+ r1 (* direction r2))]
    (assoc state
           :r1 rN
           :r2 (- (Math/sqrt (+ (* 100 100) (* 100 100))) rN))))

(defn update-state [{:keys [r1 r2] :as state}]
  (if (or (< r1 10) (< r2 10))
    (migrate-volume (update state :direction * -1))
    (migrate-volume state)))

(defn draw [{:keys [r1 r2]}]
  (q/ellipse-mode :radius)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/rotate (/ (q/frame-count) 180))
  (q/stroke 0 32)
  (q/fill 255 3)
  (q/ellipse (- r1) 0 r1 r1)
  (q/ellipse r2 0 r2 r2))

(defn ^:export run-sketch []
  (q/defsketch two-bubbles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
