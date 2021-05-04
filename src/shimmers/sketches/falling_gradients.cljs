(ns shimmers.sketches.falling-gradients
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/noise-seed (rand-int 100000))
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {})

(defn discrete-curve [slices phase scale offset]
  (for [x (range 0 1 (/ 1 slices))]
    [x (* scale (q/noise (* x phase) offset))]))

(defn draw [state]
  (q/background 1.0)
  (q/no-fill)
  (let [slices 100
        srate 0.001
        curve (discrete-curve slices 2 1.0 1000)
        depth-curve (map second (discrete-curve slices 5 1.0 50000))
        slice-width (cq/rel-w (/ 1 slices))
        slice-height (* 4 (cq/rel-h srate))]
    (q/stroke-weight (/ 50 slices))
    (doseq [[[[x1 y1] [x2 _]] depth] (map vector (partition 2 1 curve) depth-curve)]
      (q/stroke 0 1.0)
      ;; (q/line (cq/rel-pos x1 y1) (cq/rel-pos x2 y1))
      ;; (q/line (cq/rel-pos x1 y1) (cq/rel-pos x1 (+ y1 depth)))
      (q/no-stroke)
      (doseq [d (range 0.001 depth srate)
              :let [[x y] (cq/rel-pos x1 (+ y1 d))]]
        (q/fill 0 (/ 1 (q/pow 2 (* d 32))))
        (q/rect x y slice-width slice-height)))))

(defn ^:export run-sketch []
  ;; 20210504
  (q/defsketch falling-gradients
    :host "quil-host"
    :size [800 600]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
