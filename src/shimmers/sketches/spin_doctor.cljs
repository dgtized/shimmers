(ns shimmers.sketches.spin-doctor
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn displaced-triangle [triangle center expansion rotation rotate-center]
  (-> triangle
      (g/translate (tm/* (tm/- (g/centroid triangle) center) expansion))
      (geometry/rotate-around center rotation)
      (geometry/rotate-around-centroid rotate-center)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:center (cq/rel-vec 0.5 0.5)
   :radius (cq/rel-h 0.15)
   :destination (cq/rel-vec 0.8 0.3)
   :t 0})

(defn update-state [{:keys [center radius destination] :as state}]
  (let [dt (dr/random 0.5 2.0)
        p (tm/mix center destination (* dt 0.01))]
    (-> state
        (assoc
         :center p
         :destination (if (< (g/dist destination p) (* radius 0.5))
                        (cq/rel-vec (dr/random 0.2 0.8)
                                    (dr/random 0.2 0.8))
                        destination))
        (update :t + (* dt 0.1)))))

(defn noise-at [t scale p]
  (let [[x y] (tm/* p scale)]
    (q/noise x y (* t scale))))

(defn draw [{:keys [center radius t]}]
  (q/stroke-weight 0.5)
  (q/fill 1.0 0.5)
  ;; (cq/circle center radius)
  (let [expansion (+ 0.1 (* 2 (Math/sin (* 14 eq/TAU (noise-at t 0.0003 center)))))
        rotation (* 13 eq/TAU (noise-at t 0.0002 center))
        rotate-center (* 29 eq/TAU (noise-at t 0.0001 center))]
    (doseq [triangle (g/tessellate (gc/circle center radius) 10)]
      (let [tri (displaced-triangle triangle center expansion rotation rotate-center)]
        (cq/draw-polygon tri)))))

(sketch/defquil spin-doctor
  :created-at "2022-10-29"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
