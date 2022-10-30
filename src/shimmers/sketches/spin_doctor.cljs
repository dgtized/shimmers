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
   :radius (cq/rel-h 0.05)
   :destination (cq/rel-vec 0.8 0.3)
   :t 0})

(defn update-state [{:keys [center radius destination] :as state}]
  (let [dt (dr/random 0.5 2.5)
        p (tm/mix center destination (* dt 0.01))]
    (-> state
        (assoc
         :center p
         :destination (if (< (g/dist destination p) (* radius 0.5))
                        (cq/rel-vec (dr/random 0.15 0.85)
                                    (dr/random 0.15 0.85))
                        destination))
        (update :t + (* dt 0.1)))))

(defn noise-at [t scale p]
  (let [[x y] (tm/* p scale)]
    (q/noise x y (* t scale))))

(defn draw [{:keys [center radius t]}]
  (q/stroke-weight (+ 0.1 (* 0.4 (eq/unit-cos (* 11 eq/TAU (noise-at t 0.0006 center))))))
  (q/fill 1.0 0.5)
  (let [size (+ 0.75 (* 1.5 (eq/unit-sin (* eq/TAU (noise-at (+ t 7) 0.0007 center)))))
        expansion (+ 0.1 (* 2 (Math/sin (* 14 eq/TAU (noise-at t 0.0003 center)))))
        rotation (* 13 eq/TAU (noise-at t 0.0002 center))
        rotate-center (* 29 eq/TAU (noise-at t 0.0001 center))
        exp-2 (* 2 (Math/sin (* 7 eq/TAU (noise-at (+ t 20) 0.0004 center))))
        rot-2 (* 17 eq/TAU (noise-at (+ t 5) 0.00025 center))
        rot-center-2 (* 29 eq/TAU (noise-at (+ t 15) 0.00015 center))]
    (doseq [triangle (g/tessellate (gc/circle center (* size radius)) 8)]
      (let [d-triangle (displaced-triangle triangle center expansion rotation rotate-center)]
        (doseq [tri (g/subdivide d-triangle)]
          (-> tri
              (displaced-triangle (g/centroid d-triangle) exp-2 rot-2 rot-center-2)
              cq/draw-polygon))))))

(sketch/defquil spin-doctor
  :created-at "2022-10-29"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
