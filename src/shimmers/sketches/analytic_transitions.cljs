(ns shimmers.sketches.analytic-transitions
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]))

(defn cyclic [amplitude decay freq phase]
  (fn [t]
    (* (math/exp (* (- decay) t))
       amplitude
       (math/sin (+ (* freq t) phase)))))

(defrecord Pendulum [amp fx fy px py r-decay])

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
                              (mod (* 0.33 t) eq/TAU)))))
              (gv/vec2)
              plot-fs))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [a (dr/random-int 1 6)
        b (dr/random-int 1 6)]
    {:pendulums [(->Pendulum (dr/random 0.5 1.0) a b
                             (dr/random-tau) (dr/random-tau)
                             0.005)
                 (->Pendulum (dr/random 0.1 0.5) 9.01 1.0
                             (dr/random-tau) (dr/random-tau)
                             0.005)]}))

(defn update-state [state]
  state)

(defn draw [{:keys [pendulums]}]
  (q/background 1.0)
  (q/stroke-weight 2.0)
  (let [time (/ (q/millis) 3000.0)
        size (cq/rel-h 0.45)
        center (cq/rel-vec 0.5 0.5)]
    (doseq [p (plot pendulums 6000 time)]
      (let [[x y] (tm/+ center (tm/* p size))]
        (q/point x y)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition analytic-transitions
  {:created-at "2025-01-26"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
