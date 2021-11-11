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

(defonce ui-state
  (ctrl/state {:hand-drawn true
               :center-origin true}))

;; From https://www.basedesign.com/blog/how-to-render-3d-in-2d-canvas
(defn project [[x y z]]
  (let [perspective (* (q/width) 0.8)
        scale (/ perspective (+ perspective z))
        origin (tm/* (gv/vec2 (q/width) (q/height)) 0.5)]
    (-> (gv/vec2 x y)
        (tm/* scale)
        (tm/+ origin))))

(defn project-ul [[x y z]]
  (let [perspective (* (q/width) 0.8)
        scale (/ perspective (+ perspective z))]
    (tm/* (gv/vec2 x y) scale)))

(defn rotation [[x y z] [pitch yaw roll]]
  ;; From transformation A in https://en.wikipedia.org/wiki/Rotation_formalisms_in_three_dimensions
  (let [cx (Math/cos pitch)
        sx (Math/sin pitch)
        cy (Math/cos yaw)
        sy (Math/sin yaw)
        cz (Math/cos roll)
        sz (Math/sin roll)]
    (v/vec3 (+ (* x cy cz)
               (* y (+ (- (* cx sz)) (* sx sy cz)))
               (* z (+ (* sx sz) (* cx sy cz))))
            (+ (* x cy sx)
               (* y (+ (* cx cz) (* sx sy sz)))
               (* z (+ (- (* sy cz)) (* cx sy sz))))
            (+ (* x (- sy))
               (* y sx cy)
               (* z cx cy)))))

(defn rectangle [position [width height]]
  (let [hw (/ width 2)
        hh (/ height 2)]
    (map (fn [p] (v/add position p))
         [(v/vec3 (- hw) (- hh) 0)
          (v/vec3 hw (- hh) 0)
          (v/vec3 hw hh 0)
          (v/vec3 (- hw) hh 0)])))

(defn box [width height depth]
  (let [hd (/ depth 2)]
    {:vertices (concat (rectangle (gv/vec3 0 0 hd) [width height])
                       (rectangle (gv/vec3 0 0 (- hd)) [width height]))
     :edges [[0 1] [1 2] [2 3] [3 0]
             [4 5] [5 6] [6 7] [7 4]
             [0 4] [1 5] [2 6] [3 7]]}))

(defn translate-box [b offset]
  (update b :vertices
          (fn [vertices] (map (fn [p] (tm/+ p offset)) vertices))))

(defn rotate-box [b angles]
  (update b :vertices
          (fn [vertices] (map (fn [p] (rotation p angles)) vertices))))

(defn cube [[x y z] angles [width height depth]]
  (let [aabb (box width height depth)]
    (-> aabb
        (rotate-box angles)
        (translate-box (gv/vec3 x y z)))))

(comment (cube [0 0 0] [0 0 0] [10 10 10]))

(defn setup []
  (q/frame-rate 12)
  [])

(defn update-state [_]
  (let [theta (* (q/millis) 0.0005)
        offsets (cs/centered-range 3)
        s (/ (q/height) 8)
        {:keys [center-origin]} @ui-state
        origin (if center-origin 0.5 0.0)
        [x0 x1 x2] (map (fn [t] (* (- t origin) (q/width))) offsets)
        [y0 y1 y2] (map (fn [t] (* (- t origin) (q/height))) offsets)
        lower-left (gv/vec3 x0 y2 0) ;; translate relative and rotate
        lower-right (gv/vec3 x2 y2 0)] ;; rotate and then translate relative
    [(cube [x0 y0 0] [theta 0 0] [s s s])
     (cube [x1 y0 0] [0 theta 0] [s s s])
     (cube [x2 y0 0] [0 0 theta] [s s s])
     (cube [x0 y1 0] [theta theta 0] [s s s])
     (cube [x1 y1 0] [0 theta theta] [s s s])
     (cube [x2 y1 0] [theta 0 theta] [s s s])
     (-> (box (* 0.8 s) (* 0.8 s)(* 0.8 s))
         (translate-box (gv/vec3 0 0 (* 0.8 s)))
         (rotate-box [theta theta theta])
         (translate-box lower-left))
     (-> (box (* 0.5 s) (* 0.5 s) (* 0.5 s))
         (rotate-box [theta theta theta])
         (translate-box lower-left))
     (-> (box (* 0.3 s) (* 0.3 s) (* 0.3 s))
         (translate-box (gv/vec3 0 0 (* -0.6 s)))
         (rotate-box [theta theta theta])
         (translate-box lower-left))
     (cube [x1 y2 (q/lerp (* -0.5 s) (* 0.5 s) (Math/cos theta))] [0 0 0] [s s s])
     (-> (box (* 0.8 s) (* 0.8 s)(* 0.8 s))
         (rotate-box [theta theta theta])
         (translate-box (gv/vec3 0 0 (* 0.8 s)))
         (translate-box lower-right))
     (-> (box (* 0.5 s) (* 0.5 s) (* 0.5 s))
         (rotate-box [theta theta theta])
         (translate-box lower-right))
     (-> (box (* 0.3 s) (* 0.3 s) (* 0.3 s))
         (rotate-box [theta theta theta])
         (translate-box (gv/vec3 0 0 (* -0.6 s)))
         (translate-box lower-right))
     ]))

(defn draw [shapes]
  (q/background "white")
  (q/stroke "black")
  (q/stroke-weight 1)
  (let [{:keys [hand-drawn center-origin]} @ui-state
        draw-line (if hand-drawn hand-drawn/line q/line)
        projection (if center-origin project project-ul)]
    (doseq [{:keys [vertices edges]} shapes
            [a b] edges]
      (draw-line (projection (nth vertices a)) (projection (nth vertices b))))))

;; TODO: support edge occlusion if the view would be blocked by another face?
;; Add other projections like orthographic or isometric https://en.wikipedia.org/wiki/3D_projection?
(defn ui-controls []
  [:div.readable-width
   (ctrl/checkbox ui-state "Hand Drawn" [:hand-drawn])
   (ctrl/checkbox ui-state "Centered Origin" [:center-origin])
   [:p "Simple demonstration rendering 3D coordinates onto a 2D canvas using a
   vertices/edges representation of shapes. Each row demonstrates different
   kinds of rotation matrices and translations."]
   [:ol
    [:li "Single axis rotations around the x, y, and z axis."]
    [:li "2-axis rotations around [x,y], [y,z], and [x,z] axes."]
    [:li
     [:ol {:type "a" :style {:padding-left "1em"}}
      [:li "Relative translation for each cube and then simultaneous rotation around [x,y,z] axes."]
      [:li "Translation on the z axis."]
      [:li "Simultaneous rotation around [x,y,z] and then relative translation for each cube."]]]]
   [:p "\"Hand drawn\" determines if the lines should be drawn using a sketch
   like effect or with precision. \"Centered Origin\" determines if origin is in the center or upper left."]])

(sketch/defquil cube-sketch
  :created-at "2020-11-02"
  :on-mount #(ctrl/mount ui-controls)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
