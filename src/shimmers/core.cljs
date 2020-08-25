(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 30)
  {:theta 0.0})

(defn update-state [state]
  (update state :theta (fn [theta] (rem (+ theta 0.05) (* 2 Math/PI)))))

(defn circle-blob [pos rmin rmax]
  (q/with-translation pos
    (q/begin-shape)
    (loop [angle 0.0]
      (if (<= angle (* 2 Math/PI))
        (do
          (let [dt (/ (q/frame-count) 50)
                xoff (+ (q/cos angle) 1)
                yoff (+ (q/sin angle) 1)
                r (q/map-range (q/noise xoff yoff dt) 0 1 rmin rmax)
                x (* r (q/cos angle))
                y (* r (q/sin angle))]
            (q/vertex x y))
          (recur (+ angle 0.05)))))
    (q/end-shape :close)))

(defn polar-coord [theta radius x y]
  [(+ x (* radius (q/cos theta)))
   (+ y (* radius (q/sin theta)))])

(defn draw-state [{:keys [theta]}]
  (q/background 0)
  (q/stroke 255)
  (q/no-fill)
  (circle-blob (polar-coord theta 50 100 100)
               25 50)
  (circle-blob (polar-coord (+ theta 2) 25 400 400)
               25 50))

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
