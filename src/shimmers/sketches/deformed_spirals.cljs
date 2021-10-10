(ns shimmers.sketches.deformed-spirals
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

;; Concept was to make nearby spirals deform eachother like a gravity well, but
;; just doing a noise deformation was interesting on it's own.

(defn spiral [center dr dtheta steps t]
  (for [theta (range 0 (* steps dtheta) dtheta)]
    (let [pos (v/polar (* dr (/ theta tm/TWO_PI)) (+ theta (* 1.5 t)))
          [nx ny] (tm/div pos 192)
          n (q/noise nx ny t)]
      (tm/+ center pos (v/polar 40 (* n tm/TWO_PI))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 0.8)
  (q/no-fill)
  (q/begin-shape)
  (doseq [v (spiral (cq/rel-vec 0.5 0.5) 9.0 0.9 512
                    (/ (q/frame-count) 800))]
    (apply q/curve-vertex v))
  (q/end-shape))

(sketch/defquil deformed-spirals
  :created-at "2021-10-10"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
