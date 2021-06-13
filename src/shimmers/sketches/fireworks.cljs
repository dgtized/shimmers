(ns shimmers.sketches.fireworks
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
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
      (if (and (= (:type particle) :rocket)
               (< (:age particle) burn-time))
        (let [velocity (tm/- (:pos particle) (:prev particle))]
          (set! (.-mass particle) (- (:mass particle) (* mass-loss delta)))
          (tm/* (tm/normalize velocity) (* thrust delta)))
        (gv/vec2)))))

(defn max-age []
  (fn [{:keys [max-age age]} _delta]
    (< age max-age)))

(defn above-ground []
  (fn [{:keys [pos]} _delta]
    (< (:y pos) (q/height))))

;; Improve colors somehow?
(defn popper-colors []
  (rand-nth [0.0 0.05 0.35 0.6 0.8 0.9]))

(defn make-rocket [loc]
  (let [emitter (gv/vec2 loc)
        velocity (gv/vec2 (* 0.015 (q/random-gaussian)) (tm/random 0.9 1.2))]
    (assoc (make-particle emitter (tm/+ emitter velocity) 8.0)
           :type :rocket
           :hue (popper-colors)
           :max-age 600)))

;; TODO: add cracklers, and spinners
(defn make-payload [{:keys [pos prev hue]} type {:keys [quantity force max-age]}]
  (repeatedly quantity
              #(assoc (make-particle (tm/+ pos (v/jitter force)) prev 4.0)
                      :type type
                      :hue hue
                      :max-age max-age)))

(defn make-mirv [rocket quantity force]
  (make-payload rocket :mirv
                {:quantity quantity
                 :force force
                 :max-age 60}))

(defn make-bottle [rocket]
  (make-payload rocket :bottle
                {:quantity 1
                 :force (tm/random 0.1)
                 :max-age 20}))

(defn make-poppers [rocket quantity]
  (make-payload rocket :popper
                {:quantity quantity
                 :force (tm/random 0.3 0.8)
                 :max-age 60}))

(defn make-thumpers [rocket quantity]
  (make-payload rocket :thumper
                {:quantity quantity
                 :force (tm/random 0.05 0.15)
                 :max-age 42}))

;; Is there a nicer way to control this state machine per type?
(defn exploder [a b]
  (fn [{:keys [age max-age type] :as p}]
    (case type
      :rocket
      (if (p/chance (tm/smoothstep* a b (/ age max-age)))
        ((p/weighted {#(make-bottle p) 2
                      #(make-poppers p (rand-int 32)) 8
                      #(make-mirv p (int (tm/random 8 16)) (tm/random 0.5 1.1)) 3
                      #(make-thumpers p (int (tm/random 1 4))) 1}))
        [p])
      :mirv
      (if (p/chance (tm/smoothstep* 0.16 0.9 (/ age max-age)))
        (if (p/chance 0.05)
          (make-mirv p 4 (tm/random 0.5 1.1))
          (make-poppers p (int (tm/random 12 32))))
        [p])
      [p])))

(defn setup []
  ;; (q/frame-rate 2.0)
  (q/color-mode :hsl 1.0)
  (let [fps 60]
    {:system
     (make-system {:mechanics [(gravity (gv/vec2 0 (/ 9.8 fps)))
                               (solid-fuel-thruster (* 2.0 fps) 3.0 (/ 16.0 fps))]
                   :constraints [(max-age) (above-ground)]
                   :drag (/ 0.1 fps)})
     :explode (exploder 0.30 0.80)}))

(defn update-state [{:keys [system explode] :as state}]
  (when (and (< (count (:particles system)) 64)
             (p/chance (+ 0.01 (* 0.05 (q/noise 50 (q/frame-count))))))
    (let [loc (cq/rel-pos (rand-nth [0.25 0.4 0.5 0.6 0.75]) 1.0)]
      (add-particles system (repeatedly (rand-int 3) (partial make-rocket loc)))))
  (set! (.-particles system) (mapcat explode (:particles system)))
  (timestep system 2)
  state)

(defn draw-particle [{:keys [age pos hue type]}]
  (case type
    :bottle
    (let [scale (tm/random 2.0 18.0)]
      (q/fill hue (tm/random 0.3 0.9) 0.5 0.2)
      (cq/circle pos scale))
    :popper
    (let [scale (tm/random 2.0 12.0)]
      (q/fill hue (tm/random 0.3 0.9) 0.5 0.1)
      (cq/circle pos scale))
    :thumper
    (let [scale (* 42.0 (tm/smoothstep* 0.66 0.95 (/ age max-age)))]
      (q/fill 0.165 0.8 0.5 0.2)
      (cq/circle pos scale))
    :mirv
    (do (q/fill 0 0 0 0.5)
        (cq/circle pos 1.0))
    :rocket
    (do (q/fill 0 0 0 0.5)
        (cq/circle pos 1.1))))

(defn draw [{:keys [system]}]
  (q/background 1.0 0.5)
  (q/no-stroke)
  (q/ellipse-mode :radius)
  (doseq [particle (:particles system)]
    (draw-particle particle)))

;; Future: Add audio for launch shreak and crackle/explosion
;; Add fountains?
(defn ^:export run-sketch []
  ;; 20210607
  (q/defsketch fireworks
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
