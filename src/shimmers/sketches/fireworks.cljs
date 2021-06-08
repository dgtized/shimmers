(ns shimmers.sketches.fireworks
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
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

(defn max-age [ages]
  (fn [{:keys [type age]} _delta]
    (< age (get ages type))))

(defn above-ground []
  (fn [{:keys [pos]} _delta]
    (< (:y pos) (q/height))))

(defn popper-colors []
  (rand-nth [0.0 0.35 0.6 0.9]))

(defn make-rocket []
  (let [emitter (gv/vec2 (cq/rel-pos 0.5 1.0))
        velocity (gv/vec2 (* 0.01 (q/random-gaussian)) 1)]
    (assoc (make-particle emitter (tm/+ emitter velocity) 8.0)
           :type :rocket
           :hue (popper-colors))))

(defn make-mirv [{:keys [pos prev hue]}]
  (assoc (make-particle (tm/+ pos (gv/randvec2)) prev 4.0)
         :type :mirv
         :hue hue))

(defn make-poppers [{:keys [pos prev hue]} quantity]
  (repeatedly quantity
              #(assoc (make-particle (tm/+ pos (gv/randvec2)) prev 4.0)
                      :type :popper
                      :hue hue)))

(defn make-thumper [{:keys [pos prev]}]
  (assoc (make-particle (tm/+ pos (tm/* (gv/randvec2) 0.1)) prev 4.0)
         :type :thumper))

(defn exploder [a b]
  (fn [{:keys [age type] :as p}]
    (case type
      :rocket
      (if (p/chance (tm/smoothstep* a b age))
        (cond (p/chance 0.5)
              (make-poppers p (rand-int 32))
              (p/chance 0.5)
              (repeatedly (int (tm/random 8 16)) #(make-mirv p))
              :else
              (repeatedly (int (tm/random 1 4)) #(make-thumper p)))
        [p])
      :mirv
      (if (p/chance (tm/smoothstep* 10 50 age))
        (if (p/chance 0.05)
          (repeatedly 4 #(make-mirv p))
          (make-poppers p (int (tm/random 12 32))))
        [p])
      [p])))

;; How to encode particles changing state/exploding and adding new particles at
;; apogee that have different effects?
(defn setup []
  ;; (q/frame-rate 2.0)
  (q/color-mode :hsl 1.0)
  (let [fps 60]
    {:system
     (make-system {:mechanics [(gravity (gv/vec2 0 (/ 9.8 fps)))
                               (solid-fuel-thruster (* 2.0 fps) 3.0 (/ 16.0 fps))]
                   :constraints [(max-age {:rocket (* fps 20)
                                           :popper (* fps 1)
                                           :thumper (* fps 0.9)
                                           :mirv (* fps 1)})
                                 (above-ground)]
                   :drag (/ 0.1 fps)})
     :explode (exploder (* 3.5 fps) (* 7 fps))
     :draw-particle
     (fn [{:keys [age pos hue type]}]
       (let [[x y] pos]
         (q/fill 0 0 0 0.5)
         (case type
           :popper
           (let [scale (tm/random 2.0 12.0)]
             (q/fill hue (tm/random 0.3 0.9) 0.5 0.1)
             (q/ellipse x y scale scale))
           :thumper
           (let [scale (* 42.0 (tm/smoothstep* 38 48 age))]
             (q/fill 0.1 0.5 0.8 0.5)
             (q/ellipse x y scale scale))
           :mirv
           (q/ellipse x y 1.0 1.0)
           :rocket
           (q/ellipse x y 0.8 0.8))))}))

(defn update-state [{:keys [system explode] :as state}]
  (when (and (< (count (:particles system)) 64) (p/chance 0.05))
    (add-particles system (repeatedly (rand-int 3) make-rocket)))
  (set! (.-particles system) (mapcat explode (:particles system)))
  (timestep system 2)
  state)

(defn draw [{:keys [system draw-particle]}]
  (q/background 1.0 0.5)
  (q/no-stroke)
  (q/ellipse-mode :radius)
  (doseq [particle (:particles system)]
    (draw-particle particle)))

(defn ^:export run-sketch []
  ;; 20210607
  (q/defsketch fireworks
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
