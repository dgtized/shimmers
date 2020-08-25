(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn xpos [[x y]]
  x)

(defn ypos [[x y]]
  y)

(defn dist-x [[x0 _] [x1 _]]
  (- x1 x0))

(defn dist-y [[_ y0] [_ y1]]
  (- y1 y0))

(defn line-intersect [a b c d]
  (let [a1 (dist-y a b)
        b1 (dist-x a b)
        c1 (+ (* a1 (xpos a)) (* b1 (ypos a)))

        a2 (dist-y c d)
        b2 (dist-x c d)
        c2 (+ (* a2 (xpos c)) (* b2 (ypos c)))

        determinant (- (* a1 b2) (* a2 b1))]
    (when (>= (q/abs determinant) 0.0001)
      [(/ (- (* b2 c1) (* b1 c2)) determinant)
       (/ (- (* a1 c2) (* a2 c1)) determinant)])))

(defn setup []
  (q/frame-rate 30)
  {:theta 0.0})

(defn update-state [state]
  (update state :theta (fn [theta] (rem (+ theta 0.05) (* 2 Math/PI)))))

(defn angles
  "Return sequence of angles from 0.0 to 2Pi subdivided by n."
  [n]
  (take-while (fn [x] (<= x (* 2 Math/PI)))
              (iterate (partial + (/ (* 2 Math/PI) n)) 0.0)))

(defn circle-blob [[cx cy] rmin rmax]
  (for [angle (angles 10)]
    (let [dt (/ (q/frame-count) 50)
          xoff (+ (q/cos angle) 1)
          yoff (+ (q/sin angle) 1)
          r (q/map-range (q/noise xoff yoff dt) 0 1 rmin rmax)
          x (+ cx (* r (q/cos angle)))
          y (+ cy (* r (q/sin angle)))]
      [x y])))

(defn draw-shape [vertices]
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape :close))

(defn polar-coord [theta radius x y]
  [(+ x (* radius (q/cos theta)))
   (+ y (* radius (q/sin theta)))])

(defn draw-state [{:keys [theta]}]
  (q/background 0)
  (q/stroke 255)
  (q/no-fill)
  (let [shapes [(circle-blob (polar-coord theta 50 100 100)
                             25 50)
                (circle-blob (polar-coord (+ theta 2) 25 400 400)
                             25 50)]]
    (doseq [shape shapes]
      (draw-shape shape))))

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
