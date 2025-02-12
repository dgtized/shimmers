(ns shimmers.sketches.analytic-transitions
  (:require
   [clojure.math :as math]
   [fipp.ednize :refer [IEdn]]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn cyclic [amplitude decay freq phase amp-phase freq-phase phase-phase]
  (fn [t]
    (* (math/exp (* (- decay) t))
       amplitude
       (math/sin (+ (* freq eq/TAU t)
                    phase
                    (* amp-phase
                       (math/sin (+ (* freq-phase eq/TAU t)
                                    phase-phase))))))))

(defrecord Phaser [amp fx fy px py])
(defrecord Pendulum [f p r-decay])

(extend-protocol IEdn
  Pendulum
  (-edn [s]
    (tagged-literal 'Pendulum (debug/untyped s)))
  Phaser
  (-edn [s]
    (tagged-literal 'Phaser (debug/untyped s))))

(defn render [{:keys [f p r-decay]} weight]
  (let [{:keys [amp fx fy px py ]} f
        {amp1 :amp fpx :fx fpy :fy ppx :px ppy :py} p]
    (fn [theta]
      (gv/vec2 ((cyclic (/ amp weight) r-decay fx px amp1 fpx ppx) theta)
               ((cyclic (/ amp weight) r-decay fy py amp1 fpy ppy) theta)))))

(defn distribute
  "Adjust sampling distribution for theta."
  [base theta phase t]
  (let [factor (-> (math/sin (+ (* 0.00025 t)
                                (eq/cube (math/sin (+ phase
                                                      (* 0.001 theta)
                                                      (* 0.001 t))))))
                   (tm/map-interval [-1.0 1.0] [-0.66 2.0]))]
    (* (+ base theta)
       tm/PHI
       (- 1.0 (math/exp (* -0.001 (math/pow 2.0 factor)
                           (+ base theta)))))))

(defn plot [pendulums phase samples t]
  (let [weight (reduce + (mapv (fn [{:keys [f]}] (:amp f)) pendulums))
        plot-fs (mapv (fn [pendulum]
                        (render pendulum weight))
                      pendulums)
        revolutions 64
        base (* revolutions 0.25) ;; push forward so sample rate is already spread out
        ]
    (for [theta (range 0 revolutions (/ revolutions samples))
          :let [d-theta (distribute base theta phase t)]]
      (reduce (fn [p f] (tm/+ p (f d-theta)))
              (gv/vec2) plot-fs))))

(defn random-seed [amp]
  (->> (fn [] (dr/random-int (- amp) (inc amp)))
       (repeatedly 100)
       (remove (fn [v] (< (abs v) 0.1)))
       first))

(defn random-rate [m amp n]
  (->> (fn []
         (let [a (if (< amp 1)
                   1.0
                   (if (dr/chance 0.33)
                     (* (dr/random-sign)
                        (dr/weighted {0.1 1.0
                                      0.2 1.0
                                      0.25 2.0
                                      0.33 2.0
                                      0.5 3.0
                                      0.66 2.0
                                      0.75 2.0
                                      0.8 1.0
                                      0.9 1.0}))
                     (dr/random-int (- amp) (+ amp 1))))]
           (dr/gaussian (* a m) n)))
       (repeatedly 100)
       (remove (fn [v] (< (abs v) 0.1)))
       first))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [a (random-seed 6)
        b (random-seed 6)
        inner (fn []
                (->Phaser (if (dr/chance 0.75)
                            0.0
                            (dr/random 0.1 0.4))
                          (random-rate 1 3 0.005)
                          (random-rate 1 3 0.005)
                          (dr/random-tau) (dr/random-tau)))]
    {:pendulums
     [(->Pendulum (->Phaser (dr/random 0.5 1.0)
                            (random-rate a 0.0 0.005)
                            (random-rate b 0.0 0.005)
                            (dr/random-tau) (dr/random-tau))
                  (inner)
                  0.008)
      (->Pendulum (->Phaser (dr/random 0.1 0.5)
                            (random-rate a 3 0.005)
                            (random-rate b 3 0.005)
                            (dr/random-tau) (dr/random-tau))
                  (inner)
                  0.005
                  )
      (->Pendulum (->Phaser (if (dr/chance 0.35)
                              (dr/random 0.01 0.125)
                              0.0)
                            (random-rate a 5 0.01)
                            (random-rate b 5 0.01)
                            (dr/random-tau) (dr/random-tau))
                  (inner)
                  0.004)]
     :plot-phase (dr/random-tau)
     :transitions []
     :t (/ (q/millis) 1000.0)}))

(defn transition-fx [_value {:keys [kind t0 t1 freq v0 v1]} t]
  (let [pct-t (/ (- t t0) (- t1 t0))
        smooth-t (tm/smoothstep* 0.01 0.99 pct-t)]
    (case kind
      :linear
      (tm/mix* v0 v1 smooth-t)
      :sin-osc
      (tm/mix* v0 v1
               (eq/unit-sin (- (* freq eq/TAU smooth-t)
                               (* 0.25 eq/TAU)))))))

(comment
  ;; -0.25 tau push it from 0 -> 1, so initial state lines up
  ;; something is occasionally still jumping though
  (for [t (range 0 1 0.05)]
    (eq/unit-sin (- (* 1.25 eq/TAU (tm/smoothstep* 0.01 0.99 t))
                    (* 0.25 eq/TAU)))))

(defn run-effect [state {:keys [field] :as transition} t]
  (update-in state field transition-fx transition t))

(defn run-transitions [state transitions t]
  (reduce (fn [state transition]
            (run-effect state transition t))
          state
          transitions))

(defn remove-ended? [state t]
  (let [done? (fn [{:keys [t1]}] (> t t1))
        [to-remove active] (cs/separate done? (:transitions state))]
    (-> state
        (assoc :transitions active)
        (run-transitions to-remove t))))

(defn field-transition [pendulum]
  (let [kind (dr/weighted {:f 2 :p 1})
        {:keys [amp] :as phaser} (get pendulum kind)]
    (case (dr/weighted {:amp (case kind
                               :f
                               (if (<= amp 0.1) 4.0 1.0)
                               :p
                               1.0)
                        :rate 2.0
                        :phase 2.5})
      :amp
      [[kind :amp]
       (-> (cond (tm/delta= amp 0.0)
                 (dr/random 0.33)
                 (dr/chance 0.33)
                 0.0
                 :else
                 (* amp (dr/random 0.66 1.33)))
           (tm/clamp 0.0 2.0))]
      :rate
      (let [field (dr/rand-nth [:fx :fy])
            rate (get phaser field)
            new-rate (* (dr/random 0.85 1.15) rate)
            rate' (cond (and (< amp 0.1)
                             (< new-rate 10)
                             (dr/chance 0.75))
                        (tm/roundto (* (dr/random 1.5 4.0) new-rate) 1.0)
                        (and (> amp 0.15)
                             (> new-rate 16)
                             (dr/chance 0.75))
                        (tm/roundto (* (dr/random 0.1 0.66) new-rate) 1.0)
                        :else
                        new-rate)]
        [[kind field]
         (tm/clamp (if (dr/chance 0.5)
                     (tm/roundto rate' 1.0)
                     (dr/gaussian rate' 0.001))
                   0.1 48)])
      :phase
      (let [field (dr/rand-nth [:px :py])]
        [[kind field]
         (if (dr/chance 0.66)
           (dr/gaussian (get phaser field) 1.25)
           (dr/random (* 4 eq/TAU)))])
      :decay
      [[:r-decay] (dr/weighted {0.007 1.0
                                0.006 1.5
                                0.005 2.0
                                0.004 1.5
                                0.003 1.0
                                0.002 1.0})])))

(defn generate-transition [state target t]
  (let [pendulum (get-in state target)
        duration (+ (dr/random 1.0 6.0)
                    (dr/random 1.0 12.0))
        [field v1] (field-transition pendulum)
        kind (dr/weighted {:linear 2.5
                           :sin-osc 1.0})]
    {:t0 t :t1 (+ t duration)
     :field (vec (concat target field))
     :v0 (get-in pendulum field)
     :v1 v1
     :freq (/ duration (dr/random 0.66 8))
     :kind kind}))

(defn new-transition [{:keys [pendulums] :as state} t]
  (let [i (dr/random-int (count pendulums))]
    (update state :transitions conj
            (generate-transition state [:pendulums i] t))))

(defn update-state [state]
  (let [t (/ (q/millis) 1000.0)
        state' (remove-ended? state t)
        n-transitions (count (:transitions state'))]
    (-> (if (and (< n-transitions 4)
                 (dr/chance (math/pow 0.025 (+ 1 n-transitions))))
          (new-transition state' t)
          state')
        (assoc :t t))))

(defn draw [{:keys [plot-phase t] :as state}]
  (q/background 1.0)
  (q/stroke-weight 2.0)
  (reset! defo state)
  (let [size (cq/rel-h 0.5)
        center (cq/rel-vec 0.5 0.5)
        pendulums (:pendulums (run-transitions state (:transitions state) t))]
    (doseq [p (plot pendulums plot-phase 6000 t)]
      (let [[x y] (tm/+ center (tm/* p size))]
        (q/point x y)))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div {:style {:font-size "0.75em"}}
    (debug/display defo {:width 140 :print-fixed-width 3})]])

(sketch/definition analytic-transitions
  {:created-at "2025-01-26"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
