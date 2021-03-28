(ns shimmers.sketches.verlet-brushes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.physics.core :as physics]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn make-particle []
  ;; physics/particle sets initial previous to 0,0
  ;; https://github.com/thi-ng/geom/pull/81
  (let [pos (gv/vec2 (cq/rel-pos (rand) (rand)))]
    (physics/VerletParticle. pos pos (geom/clear* pos)
                             false nil nil
                             (/ 1.0 (+ 1 (* 4 (rand)))) nil)))

(defn setup []
  (let [screen-bounds (rect/rect 0 0 (q/width) (q/height))]
    {:physics
     (physics/physics
      {:particles (repeatedly 32 make-particle)
       :behaviors
       {:wind (physics/gravity (gv/vec2 1 0))
        :gravity (physics/gravity (gv/vec2 0 9.8))}
       :constraints
       {:screen-bounds (physics/shape-constraint-inside screen-bounds)}
       :drag 0.1})}))

(defn update-state [state]
  (update state :physics physics/timestep 10))

(defn draw [{:keys [physics]}]
  (doseq [{:keys [inv-weight] :as particle} (:particles physics)]
    (let [[x y] (physics/position particle)
          size (tm/map-interval inv-weight [0 1] [0.5 40])]
      (q/ellipse x y size size))))

(defn ^:export run-sketch []
  (q/defsketch verlet-brushes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
