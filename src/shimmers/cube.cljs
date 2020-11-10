(ns shimmers.cube
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.framerate :as framerate]))

(defn project [[x y z]]
  (let [perspective (* 200 0.8)
        scale (/ perspective (+ perspective z))]
    [(* scale x) (* scale y)]))

(defn rotation [[x y z] [pitch yaw roll]]
  (let [cp (q/cos pitch)
        sp (q/sin pitch)
        cy (q/cos yaw)
        sy (q/sin yaw)
        cr (q/cos roll)
        sr (q/sin roll)]
    [(+ (* cr x) (* (- sr) y))
     (+ (* sr x) (* cr y))
     z]))

(defn rectangle [[x y z] angles [width height]]
  (let [hw (/ width 2)
        hh (/ height 2)]
    (map (fn [p] (v/add (v/vec2 x y) (project (rotation p angles))))
         [(v/vec3 (- hw) (- hh) z)
          (v/vec3 hw (- hh) z)
          (v/vec3 hw hh z)
          (v/vec3 (- hw) hh z)])))

(defn cube [[x y z] angles [width height depth]]
  (let [hd (/ depth 2)]
    {:vertices (concat (rectangle [x y (+ z hd)] angles [width height])
                       (rectangle [x y (- z hd)] angles [width height]))
     :lines [[0 1] [1 2] [2 3] [3 0]
             [4 5] [5 6] [6 7] [7 4]
             [0 4] [1 5] [2 6] [3 7]]}))

(defn setup []
  {:vertices []
   :lines []})

(defn update-state [state]
  (let [theta (/ (q/frame-count) 100)]
    (cube [200 200 0] [0 0 theta] [50 50 50])))

(defn draw [{:keys [vertices lines]}]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (doseq [[a b] lines]
    (q/line (nth vertices a) (nth vertices b)))
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch cube-sketch
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


