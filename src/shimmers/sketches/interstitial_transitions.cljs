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

(defn noise-at
  ([x y offset rate]
   (let [res 192]
     (q/noise (/ x res) (/ y res)
              (+ offset (/ (q/frame-count) rate))))))

(defn shape [type x y width]
  (let [xw (+ x width)
        yw (+ y width)]
    (case type
      :triangle-left
      (q/triangle x y x yw xw yw)
      :triangle-top
      (q/triangle x y xw y x yw)
      :triangle-right
      (q/triangle x y xw y xw yw)
      :triangle-bottom
      (q/triangle xw y xw yw x yw)
      :rectangle
      (q/rect x y width width)
      :hatch
      (do
        (q/line x y xw yw)
        (q/line x yw xw y))
      :circle
      (q/ellipse x y width width))))

(defn grid [x y width divisions]
  (let [dwidth (/ width divisions)
        percent (/ width (q/width))]
    (dotimes [i divisions]
      (dotimes [j divisions]
        (let [sx (+ x (* i dwidth))
              sy (+ y (* j dwidth))
              noise (noise-at sx sy 0 400)]
          (if (> (* percent (noise-at sx sy 2000 800)) 0.1)
            (grid sx sy dwidth (cond (< noise 0.15) 8
                                     (< noise 0.35) 5
                                     (< noise 0.6) 4
                                     (< noise 0.8) 3
                                     :else 2))
            ;; adding recursive with-rotation from noise makes it appear to rotate in 3d?
            (cond (< noise 0.15) (shape :triangle-left sx sy dwidth)
                  (< noise 0.45) (shape :rectangle sx sy dwidth)
                  (< noise 0.50) (shape :triangle-top sx sy dwidth)
                  (< noise 0.54) (shape :hatch sx sy dwidth)
                  (< noise 0.58) (shape :triangle-bottom sx sy dwidth)
                  (< noise 0.90) (shape :circle sx sy dwidth)
                  :else
                  (shape :triangle-right sx sy dwidth))))))))

(defn draw []
  (q/no-fill)
  (q/background 1.0 0.2)
  (q/stroke-weight 0.9)
  (q/stroke 0 0.3)
  (grid 0 0 (q/width) 5))

(sketch/defquil interstitial-transitions
  :created-at "2021-06-26"
  :size [800 800]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
