(ns shimmers.sketches.ion-storm
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.equations :as eq]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 12)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (q/background 1.0 0.3)
  (q/ellipse-mode :radius)
  (q/stroke 0.0)
  (q/stroke-weight 3.0)

  (let [R (min (q/height) (q/width))]
    (q/fill 0.0 0.1)
    (dotimes [_ 2]
      (let [r (max (min (dr/gaussian 0.3 0.1) 0.5) 0.0)
            radius (* R r)
            hr (* radius 0.5)]
        (cq/circle
         (gc/circle (gv/vec2 (dr/random hr (- (q/width) hr))
                             (dr/random hr (- (q/height) hr)))
                    radius))))

    (q/fill 1.0 0.2)
    (dotimes [_ 6]
      (let [r (max (min (dr/gaussian 0.05 0.1) 0.3) 0.0)
            radius (* R r)
            hr (* radius 0.5)
            c (gc/circle (gv/vec2 (dr/random hr (- (q/width) hr))
                                  (dr/random hr (- (q/height) hr)))
                         radius)]
        (cq/draw-triangle (:points (triangle/inscribed-equilateral c (dr/random eq/TAU))))))))

(sketch/defquil ion-storm
  :created-at "2023-01-13"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
