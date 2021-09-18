(ns shimmers.sketches.cube
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]))

;; From https://www.basedesign.com/blog/how-to-render-3d-in-2d-canvas
(defn project [[x y z]]
  (let [perspective (* 600 0.8)
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

;; something is wrong with the z coordinates
;; I suspect the order of operations is wrong for applying translation/rotation?
(defn rectangle [[x y z] angles [width height]]
  (let [hw (/ width 2)
        hh (/ height 2)]
    (map (fn [p] (v/add (v/vec3 x y z) (rotation p angles)))
         [(v/vec3 (- hw) (- hh) 0)
          (v/vec3 hw (- hh) 0)
          (v/vec3 hw hh 0)
          (v/vec3 (- hw) hh 0)])))

(defn cube [[x y z] angles [width height depth]]
  (let [hd (/ depth 2)]
    {:vertices (concat (rectangle [x y (+ z hd)] angles [width height])
                       (rectangle [x y (- z hd)] angles [width height]))
     :lines [[0 1] [1 2] [2 3] [3 0]
             [4 5] [5 6] [6 7] [7 4]
             [0 4] [1 5] [2 6] [3 7]]}))

(defn setup []
  [])

(defn update-state [_]
  (let [fc (q/frame-count)
        theta (/ fc 100)]
    [(cube [100 100 0] [theta 0 0] [50 50 50])
     (cube [200 100 0] [0 theta 0] [50 50 50])
     (cube [300 100 0] [0 0 theta] [50 50 50])
     (cube [100 200 0] [theta theta 0] [50 50 50])
     (cube [200 200 0] [0 theta theta] [50 50 50])
     (cube [300 200 0] [theta 0 theta] [50 50 50])
     (cube [100 300 0] [theta theta theta] [30 30 30])
     (cube [100 300 50] [theta theta theta] [50 50 50])
     (cube [100 300 -50] [theta theta theta] [20 20 20])
     (cube [300 300 (q/lerp -50 50 (q/cos theta))] [0 0 0] [50 50 50])]))

(defn draw [shapes]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (doseq [{:keys [lines vertices]} shapes
          [a b] lines]
    (q/line (project (nth vertices a)) (project (nth vertices b)))))

(sketch/defquil cube-sketch
  :created-at "2020-11-02"
  :size [400 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])


