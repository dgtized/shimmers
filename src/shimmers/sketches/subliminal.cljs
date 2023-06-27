(ns shimmers.sketches.subliminal
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shape (triangle/inscribed-equilateral
               (cq/rel-vec -0.5 0.5) (cq/rel-h 0.4) 0)]
    {:t 0.0
     :shape shape
     :children (map (fn [t]
                      {:child (triangle/inscribed-equilateral
                               (gv/vec2)
                               (cq/rel-h (tm/clamp (dr/gaussian 0.06 0.03) 0.01 0.15))
                               (* eq/TAU t))
                       :offset t})
                    (dr/gaussian-range 0.05 0.02))}))

(defn slide [shape t dt]
  (g/translate
   (geometry/rotate-around-centroid shape (* (dr/gaussian 0.8 0.2) dt))
   (gv/vec2 (* (cq/rel-w 0.15) dt) 0)))

(defn update-state [{:keys [t shape] :as state}]
  (let [dt 0.01]
    (-> state
        (update :shape slide t dt)
        (update :t + dt))))

(defn draw [{:keys [shape children]}]
  (q/stroke 0.0 0.33)
  (q/no-fill)
  (q/stroke-weight (dr/gaussian 1.0 0.1))
  (if (> (:x (g/centroid shape)) (cq/rel-w 1.5))
    (q/no-loop)
    (when (dr/chance 0.4)
      #_(cq/draw-polygon shape)
      (doseq [{:keys [child offset]} children]
        (cq/draw-polygon (g/translate child (g/point-at shape offset)))))))

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
