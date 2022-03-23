(ns shimmers.sketches.voronoi-after-effect
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

;; Experimenting with moving points but drawing the delayed voronoi diagram
;; around those points.

;; FIXME: it drifts to the right for some reason
;; TODO: experiment with particle physics for the underlying points?

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0 1.0)
  (let [bounds (cq/screen-rect 0.95)]
    {:bounds bounds
     :points (rp/poisson-disc-sampling bounds 128)}))

(defn kick-point [t]
  (fn [p]
    (let [[vx vy] (tm/* p 0.01)]
      (v/wrap2d (tm/+ p (v/polar (dr/gaussian 2.0 1) (* eq/TAU (q/noise vx vy t))))
                (q/width)
                (q/height)))))

(defn perturb-points [points t]
  (dr/map-random-sample (constantly 0.025) (kick-point t) points))

(defn update-state [{:keys [t] :as state}]
  (-> state
      (update :points perturb-points t)
      (update :t + 0.01)))

(defn draw [{:keys [points]}]
  (q/stroke-weight 0.33)
  (q/stroke 0.0 0.2)
  (let [polygons (delvor/voronoi-cells points (cq/screen-rect))]
    (doseq [shape (dr/random-sample 0.05 polygons)]
      (q/no-fill)
      (when (dr/chance 0.05)
        (q/fill 1.0 0.25))
      (cq/draw-shape (g/vertices shape)))))

(sketch/defquil voronoi-after-effect
  :created-at "2022-03-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
