(ns shimmers.sketches.blobical
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.005))

(defn blob-at-t [scale theta t nr f]
  (for [s (butlast (tm/norm-range 24))]
    (let [p (v/+polar (gv/vec2 100 100) nr (* eq/TAU s))
          n (tm/smoothstep* 0.05 0.95 (apply q/noise (tm/* (gv/vec3 p t) scale)))]
      (v/polar (cq/rel-h (+ 0.05 (* 0.42 n f)))
               (* eq/TAU (+ s theta))))))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/translate (cq/rel-vec 0.5 0.5))
  (q/no-fill)
  (q/stroke 0.0)
  (let [width (+ 0.5 (eq/unit-cos (* t 0.4)))
        scale (+ 0.02 (* 0.02 (eq/unit-cos (* 0.2 t))))]
    (doseq [o (tm/norm-range 16)]
      (cq/draw-curve-shape
       (blob-at-t scale
                  (* o 0.03 width)
                  (+ t (* 0.3 o))
                  (+ 8.0 (* 6.0 width o))
                  (+ 0.75 (* 0.25 o)))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p.readable-width
    "Radial displacement for each point is determined by a polar projection
     into a 2d noise function allowing shapes to smoothly connect back to the
     start. The blob function is parameterized by time, allowing each concentric
     ring to be offset forward in time slightly, predicting where the inner
     rings will go as time steps forward."]])

(sketch/definition blobical
  {:created-at "2023-04-28"
   :type :quil
   :tags #{}}
  (ctrl/mount page))
