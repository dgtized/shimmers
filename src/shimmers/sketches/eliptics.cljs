(ns shimmers.sketches.eliptics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn generate-time-factors [n]
  (let [primes (sm/primes-between 2 50)]
    (->> (repeatedly (fn []
                       (let [num (dr/rand-nth primes)]
                         (/ num (dr/rand-nth (remove #{num} primes))))))
         (filter (fn [x] (<= 0.1 x 2.5)))
         (take n)
         vec)))

(comment (generate-time-factors 8))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (let [rates (generate-time-factors 9)
        basis (mapv * (generate-time-factors 9) (repeat eq/TAU))
        rate-base (fn [i t] (+ (nth basis i) (* t (nth rates i))))]
    {:t 0.0
     :rates rates
     :basis basis
     :rate-base rate-base}))

(defn update-state [state]
  (assoc state :t (* 0.001 (q/millis))))

(defn draw [{:keys [t rate-base]}]
  (q/background 1.0 0.15)
  (q/no-fill)
  (let [p1 (v/+polar (cq/rel-vec 0.5 0.5) (cq/rel-h 0.1) (rate-base 0 t))
        p2 (v/+polar p1 (cq/rel-h 0.1) (rate-base 1 t))
        p3 (v/+polar p2 (cq/rel-h 0.025) (rate-base 2 t))
        p2' (v/+polar p1
                      (* (cq/rel-h 0.1) (Math/sin (rate-base 4 t)))
                      (rate-base 4 t))
        p3' (v/+polar p2'
                      (* (cq/rel-h 0.05) (Math/sin (rate-base 5 t)))
                      (rate-base 6 t))
        j3  (dr/jitter (* 5 (tm/smoothstep* 0.35 0.9 (eq/unit-cos (rate-base 7 t)))))
        j3' (dr/jitter (* 4 (tm/smoothstep* 0.35 0.9 (eq/unit-cos (rate-base 8 t)))))]
    (cq/circle p1 2.0)
    (cq/circle p2 3.0)
    (cq/circle (tm/+ p3 j3) (cq/rel-h 0.1))
    (cq/circle p2' 4.0)
    (cq/circle (tm/+ p3' j3') (cq/rel-h 0.08))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition eliptics
  {:created-at "2023-08-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
