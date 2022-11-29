(ns shimmers.sketches.dreamcatcher
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shape (gc/circle (cq/rel-h 0.4))
        points (g/vertices shape 12)]
    {:shape shape
     :points (cons (first points) (reverse points))}))

(defn between-pair? [last-angle]
  (fn [[a b]]
    (when (sm/radians-between? (g/heading a) (g/heading b) last-angle)
      [a b])))

(defn next-row [points]
  (let [[last-point & preceding] points
        angle (g/heading last-point)
        preceding (take-while (fn [p] (<= (g/heading p) (+ eq/TAU angle))) preceding)]
    (mapcat (fn [[a b]] [a (tm/mix (tm/mix a b 0.5) (gv/vec2) 0.2) b])
            (partition 2 1 (conj preceding last-point)))))

(defn update-state [{:keys [points] :as state}]
  (let [row (next-row points)]
    (if (< (count points) 1000)
      (update state :points concat row)
      state)))

(defn draw [{:keys [points]}]
  (q/background 1.0)
  (q/no-fill)
  (q/with-translation (cq/rel-vec 0.5 0.5)
    (cq/draw-path points)))

(sketch/defquil dreamcatcher
  :created-at "2021-10-11"
  :on-mount (debug/mount defo)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
