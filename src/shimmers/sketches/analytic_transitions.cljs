(ns shimmers.sketches.analytic-transitions
  (:require
   [clojure.math :as math]
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
        b (dr/random-int 1 6)]
    {:pendulums [(->Pendulum (dr/random 0.5 1.0) a b
                             (dr/random-tau) (dr/random-tau)
                             0.005 [])
                 (->Pendulum (dr/random 0.1 0.5)
                             (+ (* (dr/random-int 1 6) a) (dr/gaussian 0.0 0.0075))
                             (+ (* (dr/random-int 1 6) b) (dr/gaussian 0.0 0.0075))
                             (dr/random-tau) (dr/random-tau)
                             0.005 [])]
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

(defn new-transition [pendulum t]
  (let [duration (dr/random 1.5 9.0)
        field (dr/rand-nth [:px :py])
        curr (get pendulum field)
        target (dr/random-tau)
        transition {:t0 t :t1 (+ t duration)
                    :field field
                    :fx (fn [_value pct-t]
                          (tm/mix* curr target pct-t))}]
    (update pendulum :transitions conj transition)))

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
  (let [size (cq/rel-h 0.45)
        center (cq/rel-vec 0.5 0.5)]
    (doseq [p (plot (run-transitions pendulums t) 5000 t)]
      (let [[x y] (tm/+ center (tm/* p size))]
        (q/point x y)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   (debug/display defo)])

(sketch/definition analytic-transitions
  {:created-at "2025-01-26"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
