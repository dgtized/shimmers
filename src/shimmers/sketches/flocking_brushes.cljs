(ns shimmers.sketches.flocking-brushes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.core :as sm]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.physics.core :as physics]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:trails true}))

(defn neighborhood [p particles radius]
  (filter (fn [q] (and (not= p q)
                      (< (geom/dist (physics/position p)
                                    (physics/position q))
                         radius)))
          particles))

(defn flock-alignment [verlet-physics radius strength]
  (fn [p delta]
    (let [neighborhood (neighborhood p (:particles verlet-physics) radius)]
      (when (seq neighborhood)
        (let [velocity (tm/div (reduce tm/+ (map physics/velocity neighborhood))
                               (count neighborhood))
              f (tm/- velocity (physics/velocity p))]
          (physics/add-force p (tm/* f (* strength delta))))))))

(defn flock-cohesion [verlet-physics radius strength]
  (fn [p delta]
    (let [neighborhood (neighborhood p (:particles verlet-physics) radius)]
      (when (seq neighborhood)
        (let [centroid (tm/div (reduce tm/+ (map physics/position neighborhood))
                               (count neighborhood))
              f (tm/- centroid (physics/position p))]
          (physics/add-force p (tm/* f (* strength delta))))))))

(defn flock-separation [verlet-physics radius strength]
  (fn [p delta]
    (let [neighborhood (neighborhood p (:particles verlet-physics) radius)]
      (when (seq neighborhood)
        (let [differences (map (fn [q]
                                 (let [at-p (physics/position p)
                                       at-q (physics/position q)]
                                   (tm/div (tm/- at-p at-q) (geom/dist at-p at-q))))
                               neighborhood)
              rel-diff (tm/div (reduce tm/+ differences) (count neighborhood))]
          (physics/add-force p (tm/* rel-diff (* strength delta))))))))

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
  (let [engine (physics/physics {:particles (repeatedly 48 make-particle)
                                 :drag 0.01
                                 :behaviors {:force-field (force-field 1.0)}
                                 :constraints {:wrap-around (wrap-around)}})]
    {:physics (physics/add-behaviors
               engine
               {:alignment (flock-alignment engine 48 1.0)
                :cohesion (flock-cohesion engine 64 0.05)
                :separation (flock-separation engine 32 2.5)})}))

;; Coherence/attraction - limited by some sight range?
;; Separation - how much to avoid other in flock
;; Alignment - how much to match speed/direction of flock

(defn update-state [state]
  (update state :physics physics/timestep 4))

(defn brush [particle scale]
  (let [[x y] (physics/position particle)
        [[ax ay] [bx by] [cx cy]]
        (-> (gt/triangle2 [1.5 0] [-0.5 -0.5] [-0.5 0.5])
            (geom/scale-size scale)
            (geom/rotate (geom/heading (physics/velocity particle)))
            (geom/translate (gv/vec2 x y))
            :points)]
    (q/triangle ax ay bx by cx cy)))

(defn map-noise [t rate offset interval]
  (tm/map-interval (q/noise (/ t rate) offset) [0 1] interval))

(defn superposition-coloration []
  (let [fc (q/frame-count)
        scale (tm/mix-exp 10.0 64 (q/noise (/ fc 500) 4000.0) 12)]
    (q/stroke 0 0
              (tm/smoothstep* 0.45 0.7 (q/noise (/ fc 550) 5000.0))
              (map-noise fc 650 6000.0 [0.2 0.6]))
    (q/stroke-weight (* 0.6 (tm/smoothstep* 0.35 1.0 (q/noise (/ fc 600) 0.0))))
    (q/fill (mod (* 3 (q/noise (/ fc 3000) 200.0)) 1.0)
            (map-noise fc 800 500.00 [0.4 1.0])
            (map-noise fc 800 1000.0 [0.45 1.0])
            (map-noise fc 500 2000.0 [0.001 0.040]))
    scale))

(defn draw [{:keys [physics]}]
  (let [scale (if (:trails @ui-state)
                (superposition-coloration)
                (do (q/no-fill)
                    (q/stroke-weight 0.5)
                    (q/stroke 0.0 0.5)
                    (q/background 1.0 0.2)
                    10.0))]
    (doseq [particle (:particles physics)]
      (brush particle scale))))

(defn explanation []
  [:div
   (ctrl/checkbox ui-state "Draw Trails" [:trails])])

(defn ^:export run-sketch []
  ;; 20210527
  (ctrl/mount explanation)
  (q/defsketch flocking-brushes
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
