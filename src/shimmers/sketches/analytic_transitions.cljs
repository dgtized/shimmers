(ns shimmers.sketches.analytic-transitions
  (:require
   [clojure.math :as math]
   [fipp.ednize :refer [IEdn]]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn cyclic [amplitude decay freq phase]
  (fn [t]
    (* (math/exp (* (- decay) t))
       amplitude
       (math/sin (+ (* freq t) phase)))))

(defrecord Pendulum [amp fx fy px py r-decay transitions])

(extend-protocol IEdn
  Pendulum
  (-edn [s]
    (tagged-literal 'Pendulum (debug/untyped s))))

(defn render [{:keys [amp fx fy px py r-decay]} weight]
  (fn [theta]
    (gv/vec2 ((cyclic (/ amp weight) r-decay fx px) theta)
             ((cyclic (/ amp weight) r-decay fy py) theta))))

(defn plot [pendulums p1 samples t]
  (let [weight (reduce + (mapv :amp pendulums))
        plot-fs (mapv
                 (fn [pendulum]
                   (render pendulum weight))
                 pendulums)
        revolutions (* 24 eq/TAU)
        base (* revolutions 0.1) ;; push forward so sample rate is already spread out
        ]
    (for [theta (range 0 revolutions (/ revolutions samples))]
      (reduce (fn [p f]
                (let [factor (+ 0.5 (* 2.0 (math/sin (+ (* 0.05 t)
                                                        (eq/sqr (math/sin (+ (* 0.0005 theta) p1
                                                                             (* 0.03 t))))))))]
                  (tm/+ p (f (* (+ base theta)
                                tm/PHI
                                (- 1.0 (math/exp (* -0.001
                                                    (math/pow 2 factor)
                                                    (+ base theta)))))))))
              (gv/vec2)
              plot-fs))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [a (dr/random-int 1 6)
        b (dr/random-int 1 6)
        pendulums [(->Pendulum (dr/random 0.5 1.0) a b
                               (dr/random-tau) (dr/random-tau)
                               0.005 [])
                   (->Pendulum (dr/random 0.1 0.5)
                               (+ (* (dr/random-int 1 6) a) (dr/gaussian 0.0 0.0075))
                               (+ (* (dr/random-int 1 6) b) (dr/gaussian 0.0 0.0075))
                               (dr/random-tau) (dr/random-tau)
                               0.005 [])]]
    {:pendulums (if (dr/chance 0.4)
                  (conj pendulums
                        (->Pendulum (dr/random 0.01 0.125)
                                    (+ (* (dr/random-int 1 6) a) (dr/gaussian 0.0 0.0075))
                                    (+ (* (dr/random-int 1 6) b) (dr/gaussian 0.0 0.0075))
                                    (dr/random-tau) (dr/random-tau)
                                    0.005 []))
                  pendulums)
     :p1 (dr/random-tau)
     :t (/ (q/millis) 1000.0)}))

(defn transition-fx [_value {:keys [kind t0 t1 rate v0 v1]} t]
  (let [pct-t (/ (- t t0) (- t1 t0))
        smooth-t (tm/smoothstep* 0.01 0.99 pct-t)]
    (case kind
      :linear
      (tm/mix* v0 v1 smooth-t)
      :sin-osc
      (tm/mix* v0 v1
               (eq/unit-sin (- (* rate 0.25 eq/TAU smooth-t)
                               (* 0.25 eq/TAU)))))))

(comment
  ;; -0.25 tau push it from 0 -> 1, so initial state lines up
  ;; something is occasionally still jumping though
  (for [t (range 0 1 0.05)]
    (eq/unit-sin (- (* 1.25 eq/TAU (tm/smoothstep* 0.01 0.99 t))
                    (* 0.25 eq/TAU)))))

(defn run-effect [pendulum {:keys [field] :as transition} t]
  (update pendulum field transition-fx transition t))

(defn remove-ended? [pendulums t]
  (mapv (fn [{:keys [transitions] :as pendulum}]
          (let [done? (fn [{:keys [t1]}] (> t t1))
                to-remove (filter done? transitions)]
            (-> (reduce (fn [pendulum transition]
                          (run-effect pendulum transition t))
                        pendulum
                        to-remove)
                (update :transitions (partial remove done?)))))
        pendulums))

(defn generate-transition [pendulum t]
  (let [duration (+ (dr/random 1.0 6.0)
                    (dr/random 1.0 12.0))
        [field target]
        (case (dr/weighted {:amp 0.5
                            :rate 1.0
                            :phase 1.0})
          :amp
          (let [amp (get pendulum :amp)]
            [:amp (tm/clamp (cond (tm/delta= 0.0 amp)
                                  (dr/random 0.33)
                                  (dr/chance 0.33)
                                  0.0
                                  :else
                                  (* amp (dr/random 0.66 1.33)))
                            0.0 2.0)])
          :rate
          (let [field (dr/rand-nth [:fx :fy])
                rate (* (dr/random 0.85 1.15) (get pendulum field))]
            [field
             (tm/clamp (if (dr/chance 0.5)
                         (tm/roundto rate 1.0)
                         (dr/gaussian rate 0.001))
                       0.1 32)])
          :phase
          (let [field (dr/rand-nth [:px :py])]
            [field
             (if (dr/chance 0.66)
               (dr/gaussian (get pendulum field) 1.25)
               (dr/random (* 4 eq/TAU)))]))]
    {:t0 t :t1 (+ t duration)
     :field field
     :v0 (get pendulum field)
     :v1 target
     :rate (* (dr/random 0.25 3) duration)
     :kind (dr/weighted {:linear 2.5
                         :sin-osc 1.0})}))

(defn new-transition [pendulum t]
  (update pendulum :transitions conj
          (generate-transition pendulum t)))

(defn run-transitions [pendulums t]
  (for [{:keys [transitions] :as pendulum} pendulums]
    (reduce (fn [pendulum tx] (run-effect pendulum tx t))
            pendulum transitions)))

(defn update-state [{:keys [pendulums] :as state}]
  (let [t (/ (q/millis) 1000.0)
        state' (update state :pendulums remove-ended? t)]
    (-> (if (and (every? (fn [{:keys [transitions]}] (empty? transitions))
                         pendulums)
                 (dr/chance 0.1))
          (let [n (count pendulums)
                i (dr/random-int n)]
            (update-in state' [:pendulums i] new-transition t))
          state')
        (assoc :t t))))

(defn draw [{:keys [pendulums p1 t] :as state}]
  (q/background 1.0)
  (q/stroke-weight 2.0)
  (reset! defo state)
  (let [size (cq/rel-h 0.5)
        center (cq/rel-vec 0.5 0.5)]
    (doseq [p (plot (run-transitions pendulums t) p1 6000 t)]
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
    (debug/display defo)]])

(sketch/definition analytic-transitions
  {:created-at "2025-01-26"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
