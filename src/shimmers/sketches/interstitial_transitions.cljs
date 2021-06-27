(ns shimmers.sketches.interstitial-transitions
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/noise-seed (rand-int 1000000))
  (q/frame-rate 10)
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn noise-at [x y]
  (let [res 192]
    (q/noise (/ x res) (/ y res)
             (/ (q/frame-count) 500))))

(defn grid [x y width divisions]
  (let [dwidth (/ width divisions)
        percent (/ width (q/width))]
    (dotimes [i divisions]
      (dotimes [j divisions]
        (let [sx (+ x (* i dwidth))
              sy (+ y (* j dwidth))
              noise (noise-at sx sy)]
          (if (> (* percent noise) 0.1)
            (grid sx sy dwidth (cond (< noise 0.2) 8
                                     (< noise 0.4) 5
                                     (< noise 0.6) 4
                                     (< noise 0.9) 3
                                     :else 2))
            ;; adding recursive with-rotation from noise makes it appear to rotate in 3d?
            (cond (< noise 0.2)
                  (q/triangle sx sy
                              sx (+ sy dwidth)
                              (+ sx dwidth) (+ sy dwidth))
                  (< noise 0.45)
                  (q/rect sx sy dwidth dwidth)
                  (< noise 0.50)
                  (q/triangle sx sy
                              (+ sx dwidth) sy
                              sx (+ sy dwidth))
                  (< noise 0.55)
                  (do
                    (q/line sx sy (+ sx dwidth) (+ sy dwidth))
                    (q/line sx (+ sy dwidth) (+ sx dwidth) sy))
                  (< noise 0.85)
                  (q/ellipse sx sy dwidth dwidth)
                  :else
                  (q/triangle sx sy
                              (+ sx dwidth) sy
                              (+ sx dwidth) (+ sy dwidth))
                  )))))))

(defn draw [state]
  (q/no-fill)
  (q/background 1.0 0.2)
  (q/stroke-weight 0.9)
  (q/stroke 0 0.3)
  (grid 0 0 (q/width) 5))

(sketch/defquil interstitial-transitions
  :created-at "2021-06-26"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
