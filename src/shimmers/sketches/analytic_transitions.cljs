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

(defn plot [pendulums samples t]
  (let [weight (reduce + (mapv :amp pendulums))
        plot-fs (mapv
                 (fn [pendulum]
                   (render pendulum weight))
                 pendulums)
        revolutions (* 16 eq/TAU)
        base (* revolutions 0.33) ;; push forward so sample rate is already spread out
        ]
    (for [theta (range 0 revolutions (/ revolutions samples))]
      (reduce (fn [p f]
                (tm/+ p (f (+ (math/exp (* 0.05 (+ base theta)))
                              (mod (* 0.1 t) eq/TAU)))))
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
                        (->Pendulum (dr/random 0.025 0.15)
                                    (+ (* (dr/random-int 1 6) a) (dr/gaussian 0.0 0.0075))
                                    (+ (* (dr/random-int 1 6) b) (dr/gaussian 0.0 0.0075))
                                    (dr/random-tau) (dr/random-tau)
                                    0.005 []))
                  pendulums)
     :t (/ (q/millis) 1000.0)}))

(defn remove-ended? [pendulums t]
  (mapv (fn [{:keys [transitions] :as pendulum}]
          (let [to-remove (filter (fn [{:keys [t1]}] (> t t1)) transitions)]
            (-> (reduce (fn [pendulum {:keys [field fx]}]
                          (update pendulum field fx 1.0))
                        pendulum
                        to-remove)
                (update :transitions
                        (fn [xs] (remove (fn [{:keys [t1]}] (> t t1)) xs))))))
        pendulums))

(defn generate-transition [pendulum t]
  (let [duration (+ (dr/random 1.0 6.0)
                    (dr/random 1.0 6.0))
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
          (let [field (dr/rand-nth [:fx :fy])]
            [field
             (* (dr/random 0.85 1.15) (get pendulum field))])
          :phase
          (let [field (dr/rand-nth [:px :py])]
            [field
             (dr/gaussian (get pendulum field) 1.25)]))
        curr (get pendulum field)]
    {:t0 t :t1 (+ t duration)
     :field field
     :fx (dr/weighted [[(fn [_value pct-t]
                          (tm/mix* curr target (tm/smoothstep* 0.01 0.99 pct-t))) 1.5]
                       [(let [rate (dr/random-int 25)]
                          (fn [_value pct-t]
                            (tm/mix* curr target
                                     (eq/unit-sin (- (* rate 0.25 eq/TAU (tm/smoothstep* 0.01 0.99 pct-t))
                                                     (* 0.25 eq/TAU)))))) 1.0]])}))

(comment
  ;; -0.25 tau push it from 0 -> 1, so initial state lines up
  ;; something is occasionally still jumping though
  (for [t (range 0 1 0.05)]
    (eq/unit-sin (- (* 1.25 eq/TAU (tm/smoothstep* 0.01 0.99 t))
                    (* 0.25 eq/TAU)))))

(defn new-transition [pendulum t]
  (update pendulum :transitions conj
          (generate-transition pendulum t)))

(defn run [pendulum {:keys [t0 t1 fx field]} t]
  (let [pct-t (/ (- t t0) (- t1 t0))]
    (update pendulum field fx pct-t)))

(defn run-transitions [pendulums t]
  (for [{:keys [transitions] :as pendulum} pendulums]
    (reduce (fn [pendulum tx] (run pendulum tx t)) pendulum transitions)))

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

(defn draw [{:keys [pendulums t] :as state}]
  (q/background 1.0)
  (q/stroke-weight 2.0)
  (reset! defo state)
  (let [size (cq/rel-h 0.5)
        center (cq/rel-vec 0.5 0.5)]
    (doseq [p (plot (run-transitions pendulums t) 6000 t)]
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
