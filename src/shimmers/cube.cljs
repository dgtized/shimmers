(ns shimmers.cube
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.framerate :as framerate]))

(defn project [[x y z]]
  (let [perspective (* 400 0.8)
        scale (/ perspective (+ perspective z))]
    [(* scale x) (* scale y)]))

(defn rotation [theta [x y z]]
  [(+ (* (q/cos theta) x) (* (- (q/sin theta)) y))
   (+ (* (q/sin theta) x) (* (q/cos theta) y))
   z])

(defn rectangle [[x y z] [pitch yaw roll] [width height]]
  (let [hw (/ width 2)
        hh (/ height 2)]
    (map (fn [p] (v/add (v/vec2 x y) (project (rotation pitch p))))
         [(v/vec3 (- hw) (- hh) z)
          (v/vec3 hw (- hh) z)
          (v/vec3 hw hh z)
          (v/vec3 (- hw) hh z)])))

(defn setup []
  {:vertices []})

(defn update-state [state]
  (let [theta (/ (q/frame-count) 100)]
    {:vertices (concat (rectangle [200 200 25] [theta 0 0] [50 50])
                       (rectangle [200 200 -25] [theta 0 0] [50 50]))
     :lines [[0 1] [1 2] [2 3] [3 0]
             [4 5] [5 6] [6 7] [7 4]
             [0 4] [1 5] [2 6] [3 7]]}))

(defn draw [{:keys [vertices lines]}]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (doseq [[a b] lines]
    (q/line (nth vertices a) (nth vertices b)))
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch cube
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


