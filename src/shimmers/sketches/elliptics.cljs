(ns shimmers.sketches.elliptics
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
  (let [n 15
        rates (generate-time-factors n)
        basis (mapv * (generate-time-factors n) (repeat eq/TAU))
        rate-base (fn [i t] (+ (nth basis i) (* t (nth rates i))))]
    {:t 0.0
     :rates rates
     :basis basis
     :rate-base rate-base}))

(defn update-state [state]
  (assoc state :t (* 0.001 (q/millis) (/ 5 7))))

;; invert/overlay a black circle underneath to sketch against?
(defn draw [{:keys [t rate-base]}]
  (q/background 1.0 0.15)
  (let [c (tm/smoothstep* 0.35 0.65 (eq/unit-sin (rate-base 0 (* (/ 3 13) t))))
        p1 (v/+polar (cq/rel-vec 0.5 0.5) (cq/rel-h 0.1) (rate-base 1 (* (/ 3 7) t)))
        p2 (v/+polar p1 (cq/rel-h 0.1) (rate-base 2 t))
        p3 (v/+polar p2 (cq/rel-h 0.025) (rate-base 3 t))
        p2' (v/+polar p1
                      (* (cq/rel-h 0.1) (Math/sin (rate-base 4 t)))
                      (rate-base 5 t))
        p3' (v/+polar p2'
                      (* (cq/rel-h 0.05) (Math/sin (rate-base 6 t)))
                      (rate-base 7 t))
        j3  (dr/jitter (* 5 (tm/smoothstep* 0.35 0.9 (eq/unit-cos (rate-base 8 (* (/ 2 3) t))))))
        j3' (dr/jitter (* 4 (tm/smoothstep* 0.35 0.9 (eq/unit-cos (rate-base 9 (* (/ 2 3) t))))))
        j-back (dr/jitter (* 3 (tm/smoothstep* 0.65 0.9 (eq/unit-cos (rate-base 10 (* (/ 1 3) t))))))]
    (q/stroke c (/ 2 3))
    (q/fill (- 1.0 c) (/ 1 3))
    (cq/circle (tm/+ (v/+polar p1 (cq/rel-h 0.04) (Math/sin (rate-base 11 t)))
                     j-back)
               (tm/mix* (cq/rel-h 0.3) (cq/rel-h 0.4) (eq/unit-sin (rate-base 12 (* (/ 1 7) t)))))
    (q/no-fill)
    (cq/circle (tm/+ p3 j3)
               (tm/mix* (cq/rel-h 0.05) (cq/rel-h 0.12) (eq/unit-sin (rate-base 13 (* (/ 1 3) t)))))
    (cq/circle (tm/+ p3' j3')
               (tm/mix* (cq/rel-h 0.09) (cq/rel-h 0.14) (eq/unit-sin (rate-base 14 (* (/ 1 3) t)))))
    (q/fill c (/ 1 3))
    (cq/circle p2' 4.0)
    (cq/circle p2 3.0)
    (cq/circle p1 2.0)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition elliptics
  {:created-at "2023-08-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
