(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 30))

(defn update-state [state]
  state)

(defn draw-state [{:keys [dt]}]
  (q/background 0)
  (q/stroke 255)
  (q/no-fill)
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (q/begin-shape)
    (loop [angle 0.0]
      (if (<= angle (* 2 Math/PI))
        (do
          (let [dt (/ (q/frame-count) 100)
                xoff (+ (q/cos angle) 1)
                yoff (+ (q/sin angle) 1)
                r (q/map-range (q/noise xoff yoff dt) 0 1 100 200)
                x (* r (q/cos angle))
                y (* r (q/sin angle))]
            (q/vertex x y))
          (recur (+ angle 0.05)))))
    (q/end-shape :close)))

;; this function is called in index.html
(defn ^:export run-sketch []
  (q/defsketch shimmers
    :host "shimmers"
    :size [500 500]
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))

;; uncomment this line to reset the sketch:
;; (run-sketch)
