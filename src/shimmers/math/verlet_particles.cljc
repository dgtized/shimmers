(ns shimmers.math.verlet-particles
  "Simplified version of thi.ng.geom.physics.core.VerletPhysics with slightly
  more functional approach."
  (:require [thi.ng.math.core :as tm]
            [thi.ng.geom.core :as geom]))

(defprotocol IParticle
  (pstep [_ drag force delta])
  (velocity [_]))

(defprotocol ISystem
  (add-particles [_ new-particles])
  (transform-particles [_ transformer-fn]
    "Transforms vector of particles to generate new particles or remove particles from the system.")
  (update-particles [_ delta])
  (apply-constraints [_ delta])
  (timestep [_ iter]))

(defrecord Particle [^:mutable pos
                     ^:mutable prev
                     ^:mutable age
                     ^:mutable mass]
  IParticle
  (pstep [_ drag force delta]
    (let [pos' (tm/madd force
                        (* (/ 1.0 mass) (* delta delta))
                        (tm/msub pos 2.0 prev))]
      (set! prev (tm/mix pos pos' drag))
      (set! pos pos')
      (set! age (+ age delta)))
    _)
  (velocity [_]
    (tm/- pos prev)))

(defrecord System [^:mutable particles
                   mechanics
                   constraints
                   drag]
  ISystem
  (add-particles [_ new-particles]
    (set! particles (into particles new-particles))
    _)
  (transform-particles [_ transformer-fn]
    (set! particles (mapcat transformer-fn particles)))
  (update-particles [_ delta]
    (let [drag' (* delta drag)]
      (doseq [particle (seq particles)]
        (let [force (reduce (fn [force mechanic]
                              (tm/+ force (mechanic _ particle delta)))
                            (geom/clear* (:pos particle)) ;; 2d or 3d
                            mechanics)]
          (pstep particle drag' force delta))))
    _)
  (apply-constraints [_ delta]
    (set! particles
          (filter (fn [particle] (every? #(% particle delta) constraints))
                  (seq particles)))
    _)
  (timestep [_ iter]
    (let [delta (/ 1.0 iter)]
      (dotimes [_i iter]
        (-> (update-particles _ delta)
            (apply-constraints delta))))
    _))

(defn make-particle [pos prev mass]
  (Particle. pos prev 0 mass))

(defn make-system [{:keys [particles drag mechanics constraints]
                    :or {particles [] mechanics [] constraints [] drag 0.0}}]
  (System. (vec particles) mechanics constraints drag))
