(ns shimmers.sketches.object-permanence
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  {:looking-at (gv/vec2)})

(defn mouse-position []
  (let [hx (/ (q/width) 2)
        hy (/ (q/height) 2)]
    (gv/vec2 (/ (- (q/mouse-x) hx) hx)
             (/ (- (q/mouse-y) hy) hy))))

(defn update-state [state]
  (let [mouse (mouse-position)]
    ;; (q/print-every-n-millisec 100 [state mouse])
    (update state :looking-at #(tm/normalize (tm/+ % (g/scale mouse 0.15))))))

(defn draw-eye [x y looking-at]
  (q/ellipse x y 20 16)
  (let [[lx ly] (g/scale looking-at 2)
        dx (+ x lx)
        dy (+ y ly)]
    (q/with-fill 0
      (q/ellipse dx dy 2 2))))

(defn draw [{:keys [looking-at]}]
  (q/background 255)
  (let [W (q/width)
        H (q/height)
        eye-x (/ W 14)
        eye-y (- (/ H 10))]
    (q/translate (/ W 2) (/ H 2))
    (q/rotate (q/lerp -0.02 0.02 (first looking-at)))
    (q/ellipse 0 0 (/ W 3) (/ H 1.5))
    (draw-eye (- eye-x) eye-y looking-at)
    (draw-eye eye-x eye-y looking-at)
    (let [clip 0.2]
      (q/arc 0 (/ H 8) (/ W 6) (/ H 10) clip (- tm/PI clip)))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition object-permanence
  {:created-at "2021-01-23"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
