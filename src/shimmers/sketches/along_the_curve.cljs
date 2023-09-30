(ns shimmers.sketches.along-the-curve
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.core :as sm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:points [(gv/vec2 -6 4)
            (gv/vec2 4 6)
            (gv/vec2 8 2)
            (gv/vec2 12 4)
            (gv/vec2 16 0)]})

(defn update-state [state]
  state)

(defn draw [{:keys [points]}]
  (q/background 1.0)
  (q/no-fill)
  (let [lx (:x (apply min-key :x points))
        ux (:x (apply max-key :x points))
        x-range (- ux lx)
        ly (:y (apply min-key :y points))
        uy (:y (apply max-key :y points))
        _y-range (- uy ly)
        fp (sm/lagrange-barycentric points)]
    (q/translate (/ (q/width) 2) (/ (q/height) 2))
    (q/scale 10 -10)
    (q/stroke-weight 0.05)
    (q/line -40 0 40 0)
    (q/line 0 -30 0 30)

    (q/stroke-weight 0.2)
    (q/begin-shape)
    (doseq [x (range (- lx (* x-range 0.5))
                     (+ ux (* x-range 0.5))
                     (/ (* x-range 1.5) 50.0))]
      (q/curve-vertex x (fp x)))
    (q/end-shape)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition along-the-curve
  {:created-at "2023-09-30"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
