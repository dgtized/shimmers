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
               (gv/vec2 (cq/rel-h -0.5) (cq/rel-h 0.5)) (cq/rel-h 0.4) (dr/random-tau))]
    {:t 0.0
     :shape shape
     :children (map (fn [t]
                      {:child (triangle/inscribed-equilateral
                               (gv/vec2)
                               (cq/rel-h (tm/clamp (dr/gaussian 0.038 0.03) 0.01 0.1))
                               (* eq/TAU t))
                       :shade (if (< 0.42 t 0.58)
                                [(+ 0.1 t) 0.6 0.4 0.66]
                                [(* (- 1.0 t) 0.33) 0.66])
                       :spin-rate (* (dr/weighted {-1 1 1 4})
                                     (dr/random 0.05 0.4))
                       :offset t})
                    (dr/gaussian-range 0.09 0.03))}))

(defn slide [shape _t dt]
  (g/translate
   (geometry/rotate-around-centroid shape (* (dr/gaussian 0.9 0.33) dt))
   (gv/vec2 (* (cq/rel-w 0.15) dt) 0)))

(defn update-state [{:keys [t] :as state}]
  (let [dt 0.01]
    (-> state
        (update :shape slide t dt)
        (update :t + dt))))

(defn draw [{:keys [shape children t]}]
  (if (> (:x (g/centroid shape)) (+ (q/width) (cq/rel-h 0.4)))
    (q/no-loop)
    #_(cq/draw-polygon shape)
    (doseq [{:keys [child shade offset spin-rate]} children]
      (q/no-fill)
      (apply q/stroke shade)
      (q/stroke-weight (dr/gaussian 0.75 0.33))
      (when (dr/chance 0.33)
        (-> child
            (geometry/rotate-around-centroid (* eq/TAU spin-rate t))
            (g/translate (g/point-at shape offset))
            cq/draw-polygon)))))

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
