(ns shimmers.sketches.intersection-brushes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.core :as geom]))

;; Trying out the technique Casey Reas described in
;; https://www.youtube.com/watch?v=_8DMEHxOLQE, ie move circles through space,
;; and draw a line between centers if circle intersects.

(defn make-circle []
  (let [r (* 0.05 (rand))
        x (q/random r (- 1 r))
        y (q/random r (- 1 r))]
    (assoc (gc/circle x y r)
           :velocity (tm/* (gv/randvec2) 0.005))))

(defn reflect-boundary [{:keys [p velocity] :as circle} bounds]
  (if (geom/contains-point? bounds p)
    circle
    (let [close (geom/closest-point bounds p)
          hit-y-axis (#{0 1} (:x close))
          reflected (update velocity (if hit-y-axis :x :y) -)]
      (assoc circle :velocity reflected
             :p (tm/+ close reflected)))))

(defn update-positions [circles]
  (for [{:keys [p velocity] :as circle} circles]
    (-> circle
        (update :p tm/+ velocity)
        (reflect-boundary (rect/rect)))))

(defn setup []
  {:circles (repeatedly 32 make-circle)})

(defn update-state [state]
  (update state :circles update-positions))

(defn draw [{:keys [circles]}]
  (q/background 255)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r]} circles
          :let [[x y] (cq/rel-pos p)
                radius (cq/rel-h r)]]
    (q/ellipse x y radius radius)))

(defn ^:export run-sketch []
  (q/defsketch intersection-brushes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
