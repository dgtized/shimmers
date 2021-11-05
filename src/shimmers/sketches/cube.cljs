(ns shimmers.sketches.cube
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.sequence :as cs]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

;; From https://www.basedesign.com/blog/how-to-render-3d-in-2d-canvas
;; TODO: change coordinates to center origin?
(defn project [[x y z]]
  (let [perspective (* (q/width) 0.8)
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
        theta (/ fc 100)
        s (/ (q/height) 8)
        [x0 x1 x2] (map (partial * (q/width)) (cs/centered-range 3))
        [y0 y1 y2] (map (partial * (q/height)) (cs/centered-range 3))]
    [(cube [x0 y0 0] [theta 0 0] [s s s])
     (cube [x1 y0 0] [0 theta 0] [s s s])
     (cube [x2 y0 0] [0 0 theta] [s s s])
     (cube [x0 y1 0] [theta theta 0] [s s s])
     (cube [x1 y1 0] [0 theta theta] [s s s])
     (cube [x2 y1 0] [theta 0 theta] [s s s])
     (cube [x0 y2 0] [theta theta theta] [(* 0.6 s) (* 0.6 s)(* 0.6 s)])
     (cube [x0 y2 (* 0.5 s)] [theta theta theta] [s s s])
     (cube [x0 y2 (* -0.5 s)] [theta theta theta] [(* 0.4 s) (* 0.4 s) (* 0.4 s)])
     (cube [x1 y2 (q/lerp (* -0.5 s) (* 0.5 s) (q/cos theta))] [0 0 0] [s s s])]))

(defn draw [shapes]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (doseq [{:keys [lines vertices]} shapes
          [a b] lines]
    (q/line (project (nth vertices a)) (project (nth vertices b)))))

(sketch/defquil cube-sketch
  :created-at "2020-11-02"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])


