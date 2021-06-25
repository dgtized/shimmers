(ns shimmers.sketches.object-permanence
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  {:looking-at (v/vec2 0 0)})

(defn mouse-position []
  (let [hx (/ (q/width) 2)
        hy (/ (q/height) 2)]
    (v/vec2 (/ (- (q/mouse-x) hx) hx)
            (/ (- (q/mouse-y) hy) hy))))

(defn update-state [state]
  (let [mouse (mouse-position)]
    ;; (q/print-every-n-millisec 100 [state mouse])
    (update state :looking-at #(v/normalize (v/add % (v/scale mouse 0.15))))))

(defn draw-eye [x y looking-at]
  (q/ellipse x y 20 16)
  (let [[lx ly] (v/scale looking-at 2)
        dx (+ x lx)
        dy (+ y ly)]
    (q/with-fill 0
      (q/ellipse dx dy 2 2))))

(defn draw [{:keys [looking-at]}]
  (q/background 255)
  (let [W (q/width)
        H (q/height)
        eye-x (/ W 14)
        eye-y (- (/ H 10))]
    (q/translate (/ W 2) (/ H 2))
    (q/rotate (q/lerp -0.02 0.02 (first looking-at)))
    (q/ellipse 0 0 (/ W 3) (/ H 1.5))
    (draw-eye (- eye-x) eye-y looking-at)
    (draw-eye eye-x eye-y looking-at)
    (let [clip 0.2]
      (q/arc 0 (/ H 8) (/ W 6) (/ H 10) clip (- Math/PI clip)))))

(sketch/defquil object-permanence
  :created-at "2021-01-23"
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
