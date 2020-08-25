(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn line-intersect
  "Return intersection point between two point segment pairs.

  Equations from https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [epsilon 0.000000001
        denominator (- (* (- x1 x2) (- y3 y4))
                       (* (- y1 y2) (- x3 x4)))]
    (when (>= (q/abs denominator) epsilon)
      (let [t (/ (- (* (- x1 x3) (- y3 y4))
                    (* (- y1 y3) (- x3 x4)))
                 denominator)
            u (- (/ (- (* (- x1 x2) (- y1 y3))
                       (* (- y1 y2) (- x1 x3)))
                    denominator))]
        (when (and (> t 0.0) (< t 1.0) (> u 0.0))
          [(+ x1 (* t (- x2 x1)))
           (+ y1 (* t (- y2 y1)))])))))

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
                             25 50)]
        segment (take 2 (first shapes))]

    ;; FIXME: why does order matter?
    (doseq [angle (angles 1000)]
      (let [x (* (q/width) (q/noise theta))
            y (* (q/height) (q/noise theta))
            ray [[x y] [(+ x (* 1000 (q/cos angle))) (+ y (* 1000 (q/sin angle)))]]]
        (if-let [intersection (line-intersect segment ray)]
          (q/line (first ray) intersection))))
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
