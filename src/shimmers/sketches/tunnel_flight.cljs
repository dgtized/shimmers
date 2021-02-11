(ns shimmers.sketches.tunnel-flight
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.polar :refer [angles]]
            [shimmers.math.vector :as v]))

(defn blob [base r0 r1]
  {:shape
   (for [theta (angles (rand-nth [6 8 10 12 16]))
         :let [xoff (+ 1 (q/cos theta))
               yoff (+ 1 (q/sin theta))
               r (q/map-range (q/noise xoff yoff base) 0 1
                              r0 r1)]]
     [theta r])
   :base base})

(defn setup []
  {:rings []
   :z 0})

(defn out-of-bounds? [z {:keys [base]}]
  (> (- z base) 100))

(defn add-rings [z rings]
  (if (and (< (count rings) 100)
           (> (- z (get (first rings) :base 0.0)) 10))
    (conj rings (blob z 6 10))
    rings))

(defn update-rings [z rings]
  (add-rings z (remove (partial out-of-bounds? z) rings)))

(defn update-state [{:keys [z rings] :as state}]
  (assoc (update state :z + 0.1) :rings (update-rings z rings)))

(defn scale-shape [{:keys [shape base]} z]
  (map (fn [[theta r]] (v/scale (v/unit2-from-angle theta) (+ 1 (- z base))))
       shape))

(defn draw [{:keys [z rings]}]
  (q/background 255)
  (q/no-fill)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [ring rings]
    (cq/draw-shape (scale-shape ring z))))

(defn ^:export run-sketch []
  (q/defsketch tunnel-flight
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
