(ns shimmers.sketches.fireworks
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Simplified version of thi.ng.geom.physics.core.VerletPhysics with slightly
;; more functional approach
(defprotocol IParticle
  (pstep [_ drag force delta]))

(defprotocol ISystem
  (add-particles [_ new-particles])
  (update-particles [_ delta])
  (apply-constraints [_ delta])
  (timestep [_ iter]))

(defrecord Particle [^:mutable pos
                     ^:mutable prev
                     ^:mutable age
                     inv-weight]
  IParticle
  (pstep [_ drag force delta]
    (let [pos' (tm/madd! force
                         (* inv-weight (* delta delta))
                         (tm/msub pos 2.0 prev))]
      (set! prev (tm/mix pos pos' drag))
      (set! pos pos')
      (set! age (+ age delta)))
    _))

(defrecord System [^:mutable particles
                   behaviors
                   constraints
                   drag]
  ISystem
  (add-particles [_ new-particles]
    (set! particles (into particles new-particles))
    _)
  (update-particles [_ delta]
    (let [drag' (* delta drag)]
      (doseq [particle (seq particles)]
        (let [force (reduce (fn [force behavior]
                              (tm/+ force (behavior particle delta)))
                            (geom/clear* (:pos particle)) ;; 2d or 3d
                            behaviors)]
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

(defn make-particle [pos prev weight]
  (Particle. pos prev 0 (/ 1.0 weight)))

(defn make-system [{:keys [particles drag behaviors constraints]
                    :or {particles [] behaviors [] constraints [] drag 0.0}}]
  (System. (vec particles) behaviors constraints drag))

;; Mechanics specific for this sketch

(defn gravity [force]
  (fn [_ delta]
    (tm/* force delta)))

(defn max-age [lifespan]
  (fn [{:keys [age] :as p} _delta]
    (when (< age lifespan)
      p)))

(defn above-ground []
  (fn [{:keys [pos]} _delta]
    (< (:y pos) (q/height))))

(defn make-rocket []
  (let [emitter (gv/vec2 (cq/rel-pos 0.5 1.0))
        velocity (gv/vec2 (* 1.5 (q/random-gaussian))
                          (+ 18 (* 2 (q/random-gaussian))))]
    (make-particle emitter (tm/+ emitter velocity) 1.0)))

;; How to encode particles changing state/exploding and adding new particles at
;; apogee that have different effects?
(defn setup []
  ;; (q/frame-rate 2.0)
  (q/color-mode :hsl 1.0)
  {:system (make-system {:behaviors [(gravity (gv/vec2 0 (/ 9.8 60)))]
                         :constraints [(max-age 120) (above-ground)]
                         :drag 0.02})})

(defn update-state [{:keys [system] :as state}]
  (when (< (count (:particles system)) 256)
    (add-particles system (repeatedly (rand-int 6) make-rocket)))
  (timestep system 1)
  state)

(defn draw [{:keys [system]}]
  (q/background 1.0 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [pos age]} (:particles system)
          :let [[x y] pos
                scale (tm/map-interval (tm/smoothstep* 80 120 age) 0 1 1 4)]]
    (q/ellipse x y scale scale)))

(defn ^:export run-sketch []
  ;; 20210607
  (q/defsketch fireworks
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
