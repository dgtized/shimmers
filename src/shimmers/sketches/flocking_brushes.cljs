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
  (let [factor 100
        rx (sm/reflect-into x (q/width))
        ry (sm/reflect-into y (q/height))
        n (q/noise (/ rx factor) (/ ry factor) (/ (q/frame-count) 1000))]
    (* 2 tm/TWO_PI n)))

(defn force-field []
  (fn [p delta]
    (let [[x y] (physics/position p)
          theta (direction-at-point x y)
          force (geom/as-cartesian (gv/vec2 0.5 theta))]
      (physics/add-force p (tm/* force delta)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:physics (physics/physics {:particles (repeatedly 32 make-particle)
                              :drag 0.01
                              :behaviors {:force-field (force-field)}})})

(defn update-state [state]
  (update state :physics physics/timestep 10))

(defn brush [particle]
  (let [[x y] (physics/position particle)
        [[ax ay] [bx by] [cx cy]]
        (-> (gt/triangle2 [0 0] [0 1.5] [2 0])
            (geom/scale-size 2)
            (geom/rotate (rand))
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
