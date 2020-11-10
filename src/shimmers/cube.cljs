(ns shimmers.cube
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.framerate :as framerate]))

(defn project [[x y z]]
  (let [perspective (* 400 0.8)
        scale (/ perspective (+ perspective z))]
    [(* scale x) (* scale y)]))

(defn rotation [[x y z] [pitch yaw roll]]
  ;; From transformation A in https://en.wikipedia.org/wiki/Rotation_formalisms_in_three_dimensions
  (let [cx (q/cos pitch)
        sx (q/sin pitch)
        cy (q/cos yaw)
        sy (q/sin yaw)
        cz (q/cos roll)
        sz (q/sin roll)]
    [(+ (* x cy cz) (* y (+ (- (* cx sz)) (* sx sy cz))) (* z (+ (* sx sz) (* cx sy cz))))
     (+ (* x cy sz) (* y (+ (* cx cz) (* sx sy sz))) (* z (+ (- (* sy cz)) (* cx sy sz))))
     (+ (* x (- sy)) (* y sx cy) (* z cx cy))]))

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
  (let [fc (q/frame-count)
        theta (/ fc 100)]
    [(cube [100 100 0] [theta 0 0] [50 50 50])
     (cube [200 100 0] [0 theta 0] [50 50 50])
     (cube [300 100 0] [0 0 theta] [50 50 50])
     (cube [100 200 0] [theta theta 0] [50 50 50])
     (cube [200 200 0] [0 theta theta] [50 50 50])
     (cube [300 200 0] [theta 0 theta] [50 50 50])
     (cube [100 300 0] [theta theta theta] [50 50 50])
     (cube [100 300 100] [theta theta theta] [50 50 50])
     (cube [100 300 -100] [theta theta theta] [20 20 50])
     (cube [300 300 (q/lerp -50 50 (q/cos theta))] [0 0 0] [50 50 50])]))

(defn draw [shapes]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (doseq [{:keys [lines vertices]} shapes
          [a b] lines]
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


