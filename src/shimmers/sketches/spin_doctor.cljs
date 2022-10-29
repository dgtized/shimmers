(ns shimmers.sketches.spin-doctor
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:center (cq/rel-vec 0.5 0.5)
   :radius (cq/rel-h 0.15)
   :destination (cq/rel-vec 0.8 0.3)
   :t 0})

(defn update-state [{:keys [center destination] :as state}]
  (let [p (tm/mix center destination 0.01)]
    (-> state
        (assoc
         :center p
         :destination (if (< (g/dist destination p) 10.0)
                        (cq/rel-vec (dr/random 0.2 0.8)
                                    (dr/random 0.2 0.8))
                        destination))
        (update :t + (dr/random 0.1 0.2)))))

(defn draw [{:keys [center radius t]}]
  (q/background 1.0 0.05)
  (cq/circle center radius)
  (doseq [triangle (g/tessellate (gc/circle center radius) 10)]
    (-> triangle
        (g/translate (tm/* (tm/- (g/centroid triangle) center)
                           (+ 0.1 (* 2 (Math/sin (* t 0.5))))))
        cq/draw-polygon)))

(sketch/defquil spin-doctor
  :created-at "2022-10-29"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
