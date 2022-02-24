(ns shimmers.sketches.differential-growth
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn path-split [threshold points]
  (concat (apply concat
                 (for [[p q] (partition 2 1 points)]
                   (if (> (g/dist p q) threshold)
                     [p (tm/mix p q 0.5)]
                     [p])))
          [(last points)]))

(defn path-update [{:keys [points]}]
  (gl/linestrip2 (path-split (cq/rel-w 0.03) points)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:path (gl/linestrip2 [(cq/rel-vec 0.1 0.5) (cq/rel-vec 0.9 0.5)])})

(defn update-state [state]
  (update state :path path-update))

(defn draw-path [vertices]
  (q/no-fill)
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape)
  (q/fill 0.0)
  (doseq [v vertices]
    (cq/circle v 3)))

(defn draw [{:keys [path]}]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (draw-path (:points path)))

(sketch/defquil differential-growth
  :created-at "2022-02-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
