(ns shimmers.sketches.yin-yang
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn sum-square [r1 r2]
  (+ (* r1 r1) (* r2 r2)))

(defn setup []
  {:direction 0.001
   :r1 64
   :r2 64})

(defn migrate-volume [{:keys [direction r1 r2] :as state}]
  (let [rN (+ r1 (* direction r2))]
    (assoc state
           :r1 rN
           :r2 (Math/sqrt (- (sum-square 64 64) (* rN rN))))))

(defn update-state [{:keys [r1 r2] :as state}]
  (if (or (< r1 10) (< r2 10))
    (migrate-volume (update state :direction * -1))
    (migrate-volume state)))

(defn draw [{:keys [r1 r2]}]
  (q/ellipse-mode :radius)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [fc (q/frame-count)]
    (when (zero? (mod fc 4))
      (q/rotate (/ fc 180))
      (q/stroke 0 32)
      (q/fill 255 6)
      (q/ellipse (- r1) 0 r1 r1)
      (q/ellipse r2 0 r2 r2))))

(defn ^:export run-sketch []
  (q/defsketch yin-yang
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
