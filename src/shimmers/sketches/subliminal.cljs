(ns shimmers.sketches.subliminal
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0
   :shape (triangle/inscribed-equilateral
           (cq/rel-vec -0.5 0.5) (cq/rel-h 0.4) 0)})

(defn slide [shape t dt]
  (g/translate
   (geometry/rotate-around-centroid shape (* (dr/gaussian 0.8 0.2) dt))
   (gv/vec2 (* (cq/rel-w 0.2) dt) 0)))

(defn update-state [{:keys [t shape] :as state}]
  (let [dt 0.01]
    (-> state
        (update :shape slide t dt)
        (update :t + dt))))

(defn draw [{:keys [shape]}]
  (q/stroke 0.0 0.5)
  (q/no-fill)
  (q/stroke-weight (dr/gaussian 1.0 0.1))
  #_(println shape)
  (if (> (:x (g/centroid shape)) (cq/rel-w 1.5))
    (q/no-loop)
    (when (dr/chance 0.4)
      (cq/draw-polygon shape))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition subliminal
  {:created-at "2023-06-27"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
