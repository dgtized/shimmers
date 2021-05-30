(ns shimmers.sketches.tilt
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]))

(defn stalactite []
  (let [x1 (- (* 1.1 (rand)) 0.1)
        x2 (+ x1 (* 0.09 (q/random-gaussian)))
        x3 (+ x1 (* 0.3 (rand)))
        y3 (+ (* 0.4 x3) 0.2 (* 0.03 (q/random-gaussian)))]
    (gt/triangle2 (cq/rel-pos x1 1.0)
                  (cq/rel-pos x2 1.0)
                  (cq/rel-pos x3 y3))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes (repeatedly 96 stalactite)})

(defn update-state [state]
  state)

;; Consider adding spheres of confusion somehow, ie circles or hexagon bokeh?
(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0 0.3)
  (doseq [shape shapes]
    (cq/draw-shape (geom/vertices shape))))

(defn ^:export run-sketch []
  ;; 20210530
  (q/defsketch tilt
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
