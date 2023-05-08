(ns shimmers.sketches.ring
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]))

(defn setup []
  {:theta 0.0})

(defn update-state [state]
  (update state :theta + 0.08))

(defn draw [{:keys [theta]}]
  (q/background 255 4)
  (q/stroke 10 64)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [radial-noise (q/noise (q/cos (/ theta 2)) (q/sin (/ theta 2)))
        length (cq/rel-h 0.15)
        radius (+ (cq/rel-h 0.35) (* (- radial-noise 0.5) length))
        pos (v/polar radius theta)]
    (q/stroke-weight (+ 0.8 radial-noise))
    (q/line pos
            (v/+polar pos
                      (* radial-noise length)
                      (+ theta radial-noise)))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition ring
  {:created-at "2020-12-27"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
