(ns shimmers.sketches.verlet-brushes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.physics.core :as physics]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-particle []
  ;; physics/particle sets initial previous to 0,0
  ;; https://github.com/thi-ng/geom/pull/81
  (let [pos (cq/rel-vec (rand) (rand))]
    (physics/VerletParticle. pos pos (g/clear* pos)
                             false nil nil
                             (/ 1.0 (+ 1 (* 4 (rand)))) nil)))

(defn boundary-push [boundary r strength]
  (let [rsq (* r r)]
    (fn [particle delta]
      (let [pos (physics/position particle)
            closest (g/closest-point boundary pos)
            d (tm/- pos closest)
            b (tm/cross (gv/vec3 (:x d) (:y d) 0) (gv/vec3 0 0 strength))
            l (+ (tm/mag-squared d) 1e-6)]
        (if (< l rsq)
          (physics/add-force particle (tm/* (gv/vec2 (:x b) (:y b))
                                            (/ (* (- 1.0 (/ l rsq)) (* strength delta))
                                               (Math/sqrt l))))
          (physics/add-force particle (tm/* (tm/- closest pos) 0.0001)))))))

(defn dipole [pos r1 r2 strength]
  (boundary-push (gc/circle pos r1) r2 strength))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:base-color (rand)
   :physics
   (physics/physics
    {:particles (repeatedly 64 make-particle)
     :behaviors
     {:dipoleA (dipole (cq/rel-vec 0.25 0.6) (cq/rel-h 0.1)
                       (cq/rel-h 0.4) 1.2)
      :dipoleB (dipole (cq/rel-vec 0.75 0.4) (cq/rel-h 0.1)
                       (cq/rel-h 0.4) -1.3)}
     :drag 0.05})})

(defn update-state [state]
  (update state :physics physics/timestep 10))

(defn brush [particle]
  (-> (gt/triangle2 [0 0] [0 1.5] [2 0])
      (g/scale-size 5)
      (g/rotate (rand))
      (g/translate (physics/position particle))
      cq/draw-polygon))

(defn draw [{:keys [physics base-color]}]
  ;; (q/background 1.0 0.1)
  (q/stroke 0 0.5 0.5 0.1)
  (q/stroke-weight 1)
  (q/no-stroke)
  (let [fc (q/frame-count)]
    (q/fill (mod (+ (* (Math/abs (q/cos (/ fc 1000))) 0.2) base-color) 1.0)
            (tm/map-interval (q/noise (/ fc 600) 500.0) [0 1] [0.4 0.75])
            (tm/map-interval (q/noise (/ fc 800) 1000.0) [0 1] [0.5 0.8])
            (tm/map-interval (q/noise (/ fc 500) 2000.0) [0 1] [0.01 0.1])))
  (doseq [particle (:particles physics)]
    (brush particle)))

(sketch/defquil verlet-brushes
  :created-at "2021-03-27"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
