(ns shimmers.sketches.spin-doctor
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:running true :frame-limit 2500}))

(defn displaced-triangle [triangle center expansion rotation rotate-center]
  (-> triangle
      (g/translate (tm/* (tm/- (g/centroid triangle) center) expansion))
      (geometry/rotate-around center rotation)
      (geometry/rotate-around-centroid rotate-center)))

(defn new-destination []
  (cq/rel-vec (dr/random 0.15 0.85)
              (dr/random 0.15 0.85)))

(defn setup []
  (q/noise-seed (dr/random-int 10000))
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  {:center (new-destination)
   :radius (cq/rel-h 0.05)
   :destination (new-destination)
   :velocity (gv/vec2)
   :t 0})

(defn update-state [{:keys [center radius destination velocity] :as state}]
  (let [dt (dr/random 0.5 2.5)
        direction (tm/- destination center)
        dv (tm/* direction (/ dt (tm/mag direction)))
        vel (tm/* (tm/+ velocity dv) 0.98)
        pos (tm/+ center (tm/* vel (* 0.03 dt)))
        dest (if (< (g/dist destination pos) radius)
               (new-destination)
               destination)]
    (-> state
        (assoc :center pos
               :destination dest
               :velocity vel)
        (update :t + (* dt 0.1)))))

(defn update-state-stencils [state]
  (let [dt (dr/random 0.5 2.5)]
    (-> state
        (assoc :center (new-destination))
        (update :t + (* dt 0.1)))))

(defn noise-at [t scale p]
  (let [[x y] (tm/* p scale)]
    (q/noise x y (* t scale))))

(defn draw-frame [{:keys [center radius t]}]
  (let [weight (eq/unit-cos (* 7 eq/TAU (noise-at t 0.0002 (tm/+ center (gv/vec2 5 2)))))]
    (q/stroke-weight (* 0.5 weight))
    (if (< weight 0.12)
      (q/fill (mod (* 3.0 (noise-at (* t 2) 0.0005 (tm/+ center (gv/vec2 30 40)))) 1.0)
              (+ 0.4 (* 0.5 (noise-at t 0.008 (tm/+ center (gv/vec2 30 20)))))
              (+ 0.45 (* 0.5 (noise-at t 0.009 (tm/+ center (gv/vec2 40 10)))))
              (+ 0.01 (* 4 weight)))
      (q/fill 1.0 0.5)))
  (let [size (+ 0.75 (* 1.25 (eq/unit-sin (* eq/TAU (noise-at (+ t 7) 0.0007 center)))))
        expansion (+ 0.1 (* 2 (Math/sin (* 14 eq/TAU (noise-at t 0.0003 center)))))
        rotation (* 13 eq/TAU (noise-at t 0.0002 center))
        rotate-center (* 23 eq/TAU (noise-at t 0.0001 center))
        exp-2 (* 2 (Math/sin (* 11 eq/TAU (noise-at (+ t 20) 0.0004 (tm/+ center (gv/vec2 2.0 0.5))))))
        rot-2 (* 29 eq/TAU (noise-at t 0.00025 (tm/+ center (gv/vec2 5.0 1.0))))
        rot-center-2 (* 23 eq/TAU (noise-at t 0.00015 (tm/* center 1.5)))]
    (doseq [triangle (g/tessellate (gc/circle center (* size radius)) 8)]
      (let [d-triangle (displaced-triangle triangle center expansion rotation rotate-center)]
        (doseq [tri (g/subdivide d-triangle)]
          (-> tri
              (displaced-triangle (g/centroid d-triangle) exp-2 rot-2 rot-center-2)
              cq/draw-polygon))))))

(defn draw [state]
  (let [{:keys [running frame-limit]} @ui-state]
    (when (and running (or (= 0 frame-limit) (< (q/frame-count) frame-limit)))
      (draw-frame state))))

(defn ui-controls []
  [:div
   (ctrl/checkbox-after ui-state "Running" [:running])
   (ctrl/numeric ui-state "Frame Limit" [:frame-limit] [0 100000 1000])])

(sketch/defquil spin-doctor
  :created-at "2022-10-29"
  :size [900 600]
  :on-mount (fn [] (ctrl/mount ui-controls))
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

(sketch/defquil spin-doctor-stencils
  :created-at "2022-10-30"
  :size [900 600]
  :on-mount (fn [] (ctrl/mount ui-controls))
  :setup setup
  :update update-state-stencils
  :draw draw
  :middleware [m/fun-mode framerate/mode])
