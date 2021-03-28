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

(defn position-noise []
  (fn [particle delta]
    (let [[x y] (physics/position particle)
          factor 200
          theta (q/noise (/ x factor) (/ y factor) (/ (q/frame-count) 2000))
          force (geom/as-cartesian (gv/vec2 1.0 (* 4 Math/PI theta)))]
      (physics/add-force particle (tm/* force delta)))))

(defn boundary-push [boundary r strength]
  (let [rsq (* r r)]
    (fn [particle delta]
      (let [pos (physics/position particle)
            closest (geom/closest-point boundary pos)
            d (tm/- pos closest)
            l (+ (tm/mag-squared d) 1e-6)]
        (when (< l rsq)
          (physics/add-force particle (tm/* d (/ (* (- 1.0 (/ l rsq)) (* strength delta))
                                                 (Math/sqrt l)))))))))

(defn setup []
  (let [screen-bounds (rect/rect 0 0 (q/width) (q/height))]
    {:physics
     (physics/physics
      {:particles (repeatedly 256 make-particle)
       :behaviors
       {:position-noise (position-noise)
        :boundary-push (boundary-push screen-bounds 50 2)}
       :constraints
       {:screen-bounds (physics/shape-constraint-inside screen-bounds)}
       :drag 0.15})}))

(defn update-state [state]
  (update state :physics physics/timestep 5))

(defn draw [{:keys [physics]}]
  (q/stroke-weight 0.05)
  (doseq [{:keys [inv-weight] :as particle} (:particles physics)]
    (let [[x y] (physics/position particle)
          size (tm/map-interval inv-weight [0 1] [0.2 2])]
      (q/ellipse x y size size))))

(defn ^:export run-sketch []
  (q/defsketch verlet-brushes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
