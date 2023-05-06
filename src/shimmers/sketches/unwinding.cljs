(ns shimmers.sketches.unwinding
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn remap-from [pos scale points]
  (mapv (fn [p] (g/translate (tm/* p scale) pos))
        points))

(defn plot [points]
  (q/begin-shape)
  (doseq [[x y] points]
    (q/vertex x y))
  (q/end-shape))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t (fn [t] (mod (+ t 0.005) eq/TAU))))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/translate (cq/rel-vec 0.5 0.5))
  (q/scale 1 -1)
  (q/stroke-weight 2)
  (let [scaled (fn [s] (fn [p] (tm/* p s)))
        inner
        (mapv (scaled 7)
              (eq/clothoid-from 9
                                (+ 25 (* 15 (Math/cos t)))
                                50 -1 t (gv/vec2)))
        angle (g/heading (apply tm/- (reverse (take-last 2 inner))))

        left
        (remap-from (last inner) 7
                    (eq/clothoid 10
                                 (+ 35 (* 15 (Math/sin (+ t Math/PI))))
                                 100 -1 angle (gv/vec2)))
        right
        (remap-from (last inner) 7
                    (eq/clothoid 8
                                 (+ 40 (* 15 (Math/sin t)))
                                 100 1 angle (gv/vec2)))
        big-left
        (remap-from (last inner) 13
                    (eq/clothoid (+ 11 (* 6 (Math/sin (+ t Math/PI (/ Math/PI 3)))))
                                 20
                                 60 -1 angle (gv/vec2)))
        big-right
        (remap-from (last inner) 13
                    (eq/clothoid (+ 13 (* 7 (Math/sin (+ t (/ Math/PI 3)))))
                                 25
                                 60 1 angle (gv/vec2)))]
    (doseq [base (butlast (tm/norm-range 5))]
      (q/with-rotation [(* base eq/TAU)]
        (q/stroke 0.0)
        (plot inner)
        (q/stroke 0.2)
        (plot left)
        (plot right)
        (q/stroke 0.4)
        (plot big-left)
        (plot big-right)))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition unwinding
  {:created-at "2023-01-01"
   :tags #{:genuary2023}
   :type :quil}
  (ctrl/mount page))
