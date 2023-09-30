(ns shimmers.sketches.along-the-curve
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [basis (mapv (fn [x] (gv/vec2 (int (* 50 (- x 0.5))) (dr/random-int -5 5)))
                    (dr/density-range 0.15 0.3))]
    {:basis basis
     :radius (repeatedly (count basis) #(dr/random 2.0 5.0))
     :offset (repeatedly (count basis) #(dr/random-tau))
     :t (/ (q/millis) 1000.0)}))

(defn update-state [{:keys [basis radius offset t] :as state}]
  (-> state
      (assoc :t (/ (q/millis) 1000.0))
      (assoc :points
             (mapv (fn [base r offset]
                     (v/+polar base r (+ offset (* 0.5 t))))
                   basis radius offset))))

(defn draw [{:keys [points]}]
  (q/background 1.0)
  (q/no-fill)
  (let [fp (sm/lagrange-barycentric points)]
    (q/translate (/ (q/width) 2) (/ (q/height) 2))
    (q/scale 10 -10)
    (q/stroke-weight 0.05)
    (q/line -40 0 40 0)
    (q/line 0 -30 0 30)

    (q/stroke-weight 0.2)
    (q/begin-shape)
    (doseq [x (range -50 50 (/ 100 80))]
      (let [fx (fp x)
            x' (+ x 0.1)
            fx' (fp x')
            b (gv/vec2 x fx)
            perp (g/scale (g/rotate (tm/- (gv/vec2 x' fx') b) (if (> fx fx')
                                                                (- tm/HALF_PI)
                                                                tm/HALF_PI))
                          8)]
        (q/curve-vertex x fx)
        (q/line b (tm/+ b perp))))
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
