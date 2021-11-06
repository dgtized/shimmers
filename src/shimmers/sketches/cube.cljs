(ns shimmers.sketches.cube
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.hand-drawn :as hand-drawn]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:hand-drawn false}))

;; From https://www.basedesign.com/blog/how-to-render-3d-in-2d-canvas
(defn project [[x y z]]
  (let [perspective (* (q/width) 0.8)
        scale (/ perspective (+ perspective z))]
    (v/vec2 (+ (* scale x) (* 0.5 (q/width)))
            (+ (* scale y) (* 0.5 (q/height))))))

(defn rotation [[x y z] [pitch yaw roll]]
  ;; From transformation A in https://en.wikipedia.org/wiki/Rotation_formalisms_in_three_dimensions
  (let [cx (Math/cos pitch)
        sx (Math/sin pitch)
        cy (Math/cos yaw)
        sy (Math/sin yaw)
        cz (Math/cos roll)
        sz (Math/sin roll)]
    (v/vec3 (+ (* x cy cz) (* y (+ (- (* cx sz)) (* sx sy cz))) (* z (+ (* sx sz) (* cx sy cz))))
            (+ (* x cy sz) (* y (+ (* cx cz) (* sx sy sz))) (* z (+ (- (* sy cz)) (* cx sy sz))))
            (+ (* x (- sy)) (* y sx cy) (* z cx cy)))))

(defn rectangle [position [width height]]
  (let [hw (/ width 2)
        hh (/ height 2)]
    (map (fn [p] (v/add position p))
         [(v/vec3 (- hw) (- hh) 0)
          (v/vec3 hw (- hh) 0)
          (v/vec3 hw hh 0)
          (v/vec3 (- hw) hh 0)])))

(defn cube [[x y z] angles [width height depth]]
  (let [hd (/ depth 2)]
    {:vertices (->> (concat (rectangle (gv/vec3 0 0 hd) [width height])
                            (rectangle (gv/vec3 0 0 (- hd)) [width height]))
                    (map (fn [p] (rotation p angles)))
                    (map (fn [p] (tm/+ p (gv/vec3 x y z)))))
     :lines [[0 1] [1 2] [2 3] [3 0]
             [4 5] [5 6] [6 7] [7 4]
             [0 4] [1 5] [2 6] [3 7]]}))

(comment (cube [0 0 0] [0 0 0] [10 10 10]))

(defn setup []
  (q/frame-rate 12)
  [])

(defn update-state [_]
  (let [theta (* (q/millis) 0.0005)
        offsets (cs/centered-range 3)
        s (/ (q/height) 8)
        [x0 x1 x2] (map (fn [t] (* (- t 0.5) (q/width))) offsets)
        [y0 y1 y2] (map (fn [t] (* (- t 0.5) (q/height))) offsets)]
    [(cube [x0 y0 0] [theta 0 0] [s s s])
     (cube [x1 y0 0] [0 theta 0] [s s s])
     (cube [x2 y0 0] [0 0 theta] [s s s])
     (cube [x0 y1 0] [theta theta 0] [s s s])
     (cube [x1 y1 0] [0 theta theta] [s s s])
     (cube [x2 y1 0] [theta 0 theta] [s s s])
     ;; FIXME: make these rotate along a common axis relative to this center cube
     (cube [x0 y2 0] [theta theta theta] [(* 0.6 s) (* 0.6 s)(* 0.6 s)])
     (cube [x0 y2 (* 2 s)] [theta theta theta] [s s s])
     (cube [x0 y2 (* -2 s)] [theta theta theta] [(* 0.4 s) (* 0.4 s) (* 0.4 s)])
     (cube [x1 y2 (q/lerp (* -0.5 s) (* 0.5 s) (Math/cos theta))] [0 0 0] [s s s])]))

(defn draw [shapes]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (let [draw-line (if (:hand-drawn @ui-state) hand-drawn/line q/line)]
    (doseq [{:keys [lines vertices]} shapes
            [a b] lines]
      (draw-line (project (nth vertices a)) (project (nth vertices b))))))

(defn ui-controls []
  [:div (ctrl/checkbox ui-state "Hand Drawn" [:hand-drawn])])

(sketch/defquil cube-sketch
  :created-at "2020-11-02"
  :on-mount #(ctrl/mount ui-controls)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])


