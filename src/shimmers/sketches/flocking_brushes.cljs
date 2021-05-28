(ns shimmers.sketches.flocking-brushes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.physics.core :as physics]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]
            [shimmers.math.core :as sm]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn make-particle []
  (let [pos (cq/rel-vec (rand) (rand))]
    (physics/VerletParticle. pos pos (geom/clear* pos)
                             false nil nil
                             (/ 1.0 10) nil)))

(defn direction-at-point
  [x y]
  (let [factor 500
        rx (sm/reflect-into x (q/width))
        ry (sm/reflect-into y (q/height))
        n (q/noise (/ rx factor) (/ ry factor) (/ (q/frame-count) 1000))]
    (* 2 tm/TWO_PI n)))

(defn force-field [strength]
  (fn [p delta]
    (let [[x y] (physics/position p)
          theta (direction-at-point x y)
          force (geom/as-cartesian (gv/vec2 strength theta))]
      (physics/add-force p (tm/* force delta)))))

(defn wrap-around []
  (fn [p _]
    (let [[x y] (physics/position p)
          wx (tm/wrap-range x (q/width))
          wy (tm/wrap-range y (q/height))]
      (when-not (and (tm/delta= x wx) (tm/delta= y wy))
        (let [pos (gv/vec2 wx wy)]
          (set! (.-prev p) (tm/- pos (physics/velocity p)))
          (physics/set-position p pos))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:physics (physics/physics {:particles (repeatedly 32 make-particle)
                              :drag 0.01
                              :behaviors {:force-field (force-field 0.8)}
                              :constraints {:wrap-around (wrap-around)}})})

;; Coherence/attraction - limited by some sight range?
;; Separation - how much to avoid other in flock
;; Alignment - how much to match speed/direction of flock

(defn update-state [state]
  (update state :physics physics/timestep 4))

(defn brush [particle]
  (let [[x y] (physics/position particle)
        [[ax ay] [bx by] [cx cy]]
        (-> (gt/triangle2 [0 0] [0 3] [5 0])
            (geom/rotate (/ (q/frame-count) 20))
            (geom/translate (gv/vec2 x y))
            :points)]
    (q/triangle ax ay bx by cx cy)))

(defn draw [{:keys [physics]}]
  (q/stroke 0.0 0.1)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (doseq [particle (:particles physics)]
    (brush particle)))

(defn ^:export run-sketch []
  ;; 20210527
  (q/defsketch flocking-brushes
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
