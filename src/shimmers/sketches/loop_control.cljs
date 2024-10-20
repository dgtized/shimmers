(ns shimmers.sketches.loop-control
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]
   [clojure.math :as math]))

(defn random-equation []
  (dr/weighted
   [[(fn [c _t] (* 0.5 (+ c 1))) 1.0]
    [(fn [c _t] (* 0.5 (math/exp (+ 1 c)))) 1.0]
    [(fn [c _t] (* 0.5 (math/pow 3 (+ 1 c)))) 1.0]
    ;; FIXME why is this reversing?
    [(fn [c t] (+ 1.0 (* 0.5 (math/sin (+ t (* eq/TAU c)))))) 1.0]]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0
   :t0 0
   :transition (dr/random 16.0 32.0)
   :rate-fn (random-equation)
   :rate-fn' (random-equation)
   :circles
   (into []
         (for [c (butlast (tm/norm-range 24))]
           {:p (v/+polar (cq/rel-vec 0.5 0.5) (cq/rel-h 0.38) (* c eq/TAU))
            :c c}))})

(defn update-state [{:keys [t0 transition rate-fn] :as state}]
  (let [t (/ (q/millis) 1000.0)
        state' (assoc state :t t)]
    (if (> t (+ t0 transition))
      (assoc state'
             :t0 t
             :transition (dr/random 30.0 60.0)
             :rate-fn (random-equation)
             :rate-fn' rate-fn)
      state')))

(defn draw [{:keys [circles rate-fn rate-fn' transition t0 t]}]
  (q/background 1.0)
  (let [r (cq/rel-h 0.05)
        mix (tm/map-interval-clamped t [t0 (+ t0 (* 0.2 transition))] [0 1])]
    (doseq [{:keys [p c]} circles
            :let [r0 (rate-fn' c t)
                  r1 (rate-fn c t)
                  rate (tm/mix* r0 r1 mix)
                  theta (* rate eq/TAU t)
                  p2 (v/+polar p r theta)]]
      (cq/circle p 2.0)
      (q/line p p2)
      (cq/circle p2 2.0))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition loop-control
  {:created-at "2024-10-19"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
