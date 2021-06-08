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
                     ^:mutable mass]
  IParticle
  (pstep [_ drag force delta]
    (let [pos' (tm/madd force
                        (* (/ 1.0 mass) (* delta delta))
                        (tm/msub pos 2.0 prev))]
      (set! prev (tm/mix pos pos' drag))
      (set! pos pos')
      (set! age (+ age delta)))
    _))

(defrecord System [^:mutable particles
                   mechanics
                   constraints
                   drag]
  ISystem
  (add-particles [_ new-particles]
    (set! particles (into particles new-particles))
    _)
  (update-particles [_ delta]
    (let [drag' (* delta drag)]
      (doseq [particle (seq particles)]
        (let [force (reduce (fn [force mechanic]
                              (tm/+ force (mechanic particle delta)))
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

;; Mechanics specific for this sketch

(defn gravity [force]
  (fn [_ delta]
    (tm/* force delta)))

(defn solid-fuel-thruster [burn-time fuel-mass thrust]
  (let [mass-loss (/ fuel-mass burn-time)]
    (fn [particle delta]
      (if (< (:age particle) burn-time)
        (let [velocity (tm/- (:pos particle) (:prev particle))]
          (set! (.-mass particle) (- (:mass particle) (* mass-loss delta)))
          (tm/* (tm/normalize velocity) (* thrust delta)))
        (gv/vec2)))))

(defn max-age [lifespan]
  (fn [{:keys [age] :as p} _delta]
    (when (< age lifespan)
      p)))

(defn above-ground []
  (fn [{:keys [pos]} _delta]
    (< (:y pos) (q/height))))

(defn make-rocket []
  (let [emitter (gv/vec2 (cq/rel-pos 0.5 1.0))
        velocity (gv/vec2 (* 0.01 (q/random-gaussian)) 1)]
    (make-particle emitter (tm/+ emitter velocity) 8.0)))

;; How to encode particles changing state/exploding and adding new particles at
;; apogee that have different effects?
(defn setup []
  ;; (q/frame-rate 2.0)
  (q/color-mode :hsl 1.0)
  (let [fps 60]
    {:system (make-system {:mechanics [(gravity (gv/vec2 0 (/ 9.8 fps)))
                                       (solid-fuel-thruster (* 2.0 fps) 3.0 (/ 20.0 fps))]
                           :constraints [(max-age (* fps 20)) (above-ground)]
                           :drag (/ 0.1 fps)})
     :sizer (fn [age] (tm/map-interval (tm/smoothstep* (* 2 fps) (* 3 fps) age)
                                      0 1 1 4))}))

(defn update-state [{:keys [system] :as state}]
  (when (< (count (:particles system)) 256)
    (add-particles system (repeatedly (rand-int 6) make-rocket)))
  (timestep system 2)
  state)

(defn draw [{:keys [system sizer]}]
  (q/background 1.0 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [pos age]} (:particles system)
          :let [[x y] pos
                scale (sizer age)]]
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
