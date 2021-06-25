(ns shimmers.sketches.falling-gradients
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]))

(defn setup []
  (q/noise-seed (rand-int 100000))
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {})

(defn discrete-curve [slices phase scale offset]
  (for [x (range 0 1 (/ 1 slices))]
    [x (* scale (q/noise (* x phase) offset))]))

(defn random-triangle-at [pos rotation scale]
  (-> (gt/triangle2 [0 0] [0.2 0.8] [1.0 0.1])
      (geom/rotate rotation)
      (geom/scale-size scale)
      (geom/translate pos)))

(defn draw [state]
  (q/background 1.0)
  (q/no-fill)
  (let [slices 100
        curve (discrete-curve slices 2 0.4 1000)
        depth-curve (map second (discrete-curve slices 5 1.0 50000))
        slice-width (cq/rel-w (/ 1 slices))]
    (q/stroke-weight (/ 50 slices))
    (doseq [[[x1 y1] depth] (map vector curve depth-curve)
            :let [theta (* 2 Math/PI (rand))]]
      (q/no-stroke)
      (let [f (q/random -0.0075 -0.0125)]
        (doseq [s (range 400)
                :let [d (* depth (Math/pow Math/E (* f s)))]]
          (q/fill 0.2 0.008)
          (-> (cq/rel-pos x1 (+ y1 d))
              (random-triangle-at (+ theta (* 2 Math/PI d))
                                  (* 2 slice-width))
              geom/vertices
              cq/draw-shape))))))

(sketch/defquil falling-gradients
  :created-at "2021-05-04"
  :size [800 600]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
