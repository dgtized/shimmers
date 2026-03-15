(ns shimmers.sketches.spiral-loops
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
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

;; trying to mimic: https://www.instagram.com/reel/DTdq_8ZEu-p/
(defn spiral [n t]
  (abs (/ (math/pow n (/ 3.0 2.0))
          (+ n
             (* 1000 (math/sin (* 0.1 n (math/sin (* 83.33 t)))))
             (* 0.1 n t)))))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (dr/random 0.0 4000.0)
   :zoom 1.0})

(defn sample [n sample-rate theta t]
  (spiral n (+ (* sample-rate theta) (* 0.08 t))))

(defn average [xs]
  (/ (reduce + xs) (count xs)))

(defn weighted-avg [sample avg k]
  (+ (* k sample) (* (- 1.0 k) avg)))

(defn calculate-zoom [n limit sample-rate t]
  (let [samples (repeatedly 16 (fn [] (sample n sample-rate (dr/random limit) t)))
        largest (average (take 3 (sort > samples)))]
    (/ 1.0 largest)))

(defn update-zoom [{:keys [n limit sample-rate t] :as state}]
  (update state :zoom
          (fn [zoom] (weighted-avg (calculate-zoom n limit sample-rate t) zoom 0.001))))

(defn update-state [{:keys [t] :as state}]
  (-> state
      (assoc :n (+ 50 (* 48 (math/sin (* 0.3 eq/TAU t))))
             :limit (+ eq/TAU (* 16 eq/TAU (eq/unit-sin (* eq/TAU (- (* 0.2 t) 0.25)))))
             :sample-rate (+ 0.008 (* 0.007 (math/sin (* 0.01 t)))))
      update-zoom
      (update :t + 0.001)))

(defn draw [{:keys [t n limit sample-rate zoom]}]
  (q/background 1.0)
  (let [c (cq/rel-vec 0.5 0.5)]
    (reset! defo {:t t
                  :n n
                  :limit limit
                  :sample-rate sample-rate
                  :zoom zoom})
    (q/begin-shape)
    (doseq [theta (range 0 limit 0.02)]
      (let [[x y] (v/+polar c
                            (* zoom (cq/rel-h 0.48)
                               (sample n sample-rate theta t))
                            theta)]
        (q/vertex x y)))
    (q/end-shape)))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   (debug/display defo)])

(sketch/definition spiral-loops
  {:created-at "2026-02-05"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
