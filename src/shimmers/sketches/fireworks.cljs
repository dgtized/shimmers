(ns shimmers.sketches.fireworks
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.common.quil :as cq]))

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
                     ^:mutable lifespan
                     inv-weight]
  IParticle
  (pstep [_ drag force delta]
    (let [pos' (tm/madd! force
                         (* inv-weight (* delta delta))
                         (tm/msub pos 2.0 prev))]
      (set! prev (tm/mix pos pos' drag))
      (set! pos pos')
      (set! lifespan (- lifespan delta)))
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
                            (gv/vec2) ;; 2d/3d switch?
                            behaviors)]
          (pstep particle drag' force delta))))
    _)
  (apply-constraints [_ delta]
    (set! particles
          (filter (fn [particle] (every? #(% particle delta) constraints))
                  (seq particles))))
  (timestep [_ iter]
    (let [delta (/ 1.0 iter)]
      (dotimes [_i iter]
        (update-particles _ delta)
        (apply-constraints _ delta)))
    _))

(defn gravity [force]
  (fn [_ delta]
    (tm/* force delta)))

(defn max-lifespan []
  (fn [{:keys [lifespan] :as p} delta]
    (when (> lifespan delta)
      p)))

(defn above-ground []
  (fn [{:keys [pos]} _delta]
    (< (:y pos) (q/height))))

(defn make-particle [pos prev lifespan weight]
  (Particle. pos prev lifespan (/ 1.0 weight)))

(defn make-system [{:keys [particles drag behaviors constraints]
                    :or {particles [] behaviors [] constraints [] drag 0.0}}]
  (System. (vec particles) behaviors constraints drag))

(defn setup []
  ;; (q/frame-rate 2.0)
  (q/color-mode :hsl 1.0)
  {:system (make-system {:behaviors [(gravity (gv/vec2 0 (/ 9.8 60)))]
                         :constraints [(max-lifespan) (above-ground)]
                         :drag 0.05})})

(defn update-state [{:keys [system] :as state}]
  (when (< (count (:particles system)) 100)
    (add-particles system
                   (repeatedly (rand-int 8)
                               #(let [emitter (apply gv/vec2 (cq/rel-pos 0.5 1.0))
                                      velocity (tm/+ emitter (gv/vec2 (* 0.5 (tm/randnorm)) 25.0))]
                                  (make-particle emitter velocity 360 1.0)))))
  (timestep system 1)
  state)

(defn draw [{:keys [system]}]
  (q/background 1.0 0.5)
  (q/ellipse-mode :radius)
  (doseq [particle (:particles system)
          :let [[x y] (:pos particle)]]
    (q/ellipse x y 1.0 1.0)))

(defn ^:export run-sketch []
  ;; 20210607
  (q/defsketch fireworks
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
