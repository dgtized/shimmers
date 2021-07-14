(ns shimmers.sketches.tunnel-flight
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.core :refer [angles]]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]))

(defn blob [base r0 r1]
  (let [seed (* base 0.05)]
    {:position
     (v/sub (v/vec2 (q/noise seed 5) (q/noise seed 10))
            (v/vec2 0.5 0.5))
     :shape
     (for [theta (angles 10)
           :let [xoff (+ 1 (q/cos theta))
                 yoff (+ 1 (q/sin theta))
                 r (q/map-range (q/noise xoff yoff (* 3 seed)) 0 1
                                r0 r1)]]
       [theta r])
     :base base}))

(defn setup []
  {:rings []
   :z 0})

(defn out-of-bounds? [z {:keys [base]}]
  (> (- z base) 32))

(defn add-rings [z rings]
  (if (and (< (count rings) 100)
           (> (- z (get (first rings) :base 0.0)) 2.0))
    (conj rings (blob z 0.3 0.8))
    rings))

(defn update-state [{:keys [z rings] :as state}]
  (assoc (update state :z + 0.11)
         :rings (add-rings z (remove (partial out-of-bounds? z) rings))))

(defn scale-shape [{:keys [shape base]} z]
  (map (fn [[theta r]]
         (v/polar (* r (q/pow 2 (/ (- z base) 3.0))) theta))
       shape))

(defn draw [{:keys [z rings]}]
  (q/background 255 48)
  (let [noise (q/noise (* 0.1 z))]
    (q/stroke (* 100 noise) 50 (* 150 noise) 250))
  (q/stroke-weight 0.25)
  (q/no-fill)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/rotate (* 2 Math/PI (q/noise (* 0.05 z) 30)))
  (doseq [ring rings]
    (q/push-matrix)
    (apply q/translate (v/scale (:position ring) (* 4 (- z (:base ring)))))
    (cq/draw-shape (scale-shape ring z))
    (q/pop-matrix)))

(sketch/defquil tunnel-flight
  :created-at "2021-02-10"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
