(ns shimmers.sketches.eliptics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:t 0.0})

(defn update-state [state]
  (assoc state :t (* 0.001 (q/millis))))

(defn draw [{:keys [t]}]
  (q/background 1.0 0.15)
  (q/no-fill)
  (let [p1 (v/+polar (cq/rel-vec 0.5 0.5) (cq/rel-h 0.1) (* t 1))
        p2 (v/+polar p1 (cq/rel-h 0.1) (* t (/ 5 3)))
        p3 (v/+polar p2 (cq/rel-h 0.025) (* t (/ 7 2)))
        p2' (v/+polar p1
                      (* (cq/rel-h 0.1) (Math/sin (* t (/ 5 7))))
                      (* t (/ 11 3)))
        p3' (v/+polar p2'
                      (* (cq/rel-h 0.05) (Math/sin (* t (/ 3 5))))
                      (* t (/ 13 7)))
        j3  (dr/jitter (* 5 (tm/smoothstep* 0.35 0.9 (eq/unit-cos (* t (/ 4 7))))))
        j3' (dr/jitter (* 4 (tm/smoothstep* 0.35 0.9 (eq/unit-cos (* t (/ 5 11))))))]
    (cq/circle p1 2.0)
    (cq/circle p2 3.0)
    (cq/circle (tm/+ p3 j3) (cq/rel-h 0.1))
    (cq/circle p2' 4.0)
    (cq/circle (tm/+ p3' j3') (cq/rel-h 0.08))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition eliptics
  {:created-at "2023-08-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
