(ns shimmers.sketches.rose
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.polar :refer [angles]]
            [shimmers.math.vector :as v]))

(defn blob [base r0 r1]
  {:shape
   (for [theta (angles (rand-nth [8 12 16 20]))
         :let [xoff (+ 1 (q/cos theta))
               yoff (+ 1 (q/sin theta))
               r (q/map-range (q/noise xoff yoff base) 0 1
                              r0 r1)]]
     [theta r])
   :base base})

(defn setup []
  {:rings []
   :z 0})

(defn scale [z base]
  (q/sin (/ (- z base) 20.0)))

(defn out-of-bounds? [z {:keys [base]}]
  (< (scale z base) 0.0))

(defn add-rings [z rings]
  (if (and (< (count rings) 100)
           (> (- z (get (first rings) :base 0.0)) 6.0))
    (conj rings (blob z 0.1 0.8))
    rings))

(defn update-state [{:keys [z rings] :as state}]
  (assoc (update state :z + 0.05)
         :rings (add-rings z (remove (partial out-of-bounds? z) rings))))

(defn scale-shape [{:keys [shape base]} z]
  (map (fn [[theta r]]
         (v/scale (v/unit2-from-angle theta)
                  (* r 192.0 (scale z base))))
       shape))

(defn draw [{:keys [z rings]}]
  (q/background 255 (rand-nth [2.0 4.0 4.0 16.0]))
  (q/stroke 200 50 50 255)
  (q/stroke-weight 0.25)
  (q/no-fill)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [ring rings]
    (cq/draw-curve-shape (scale-shape ring z))))

(defn ^:export run-sketch []
  (q/defsketch rose
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
