(ns shimmers.sketches.tunnel-flight
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.polar :refer [angles]]
            [shimmers.math.vector :as v]))

(defn blob [z r0 r1]
  (for [theta (angles 10)
        :let [zmod (+ 0.5 (mod z 3))
              xoff (+ 1 (q/cos theta))
              yoff (+ 1 (q/sin theta))
              r (q/map-range (q/noise xoff yoff z) 0 1
                             (* zmod r0) (* zmod r1))]]
    (v/scale (v/unit2-from-angle theta) r)))

(defn setup []
  {:z 0.0})

(defn update-state [state]
  (update state :z + 0.005))

(defn draw [{:keys [z]}]
  (q/background 255)
  (q/no-fill)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [base (range 0 100 10)]
    (cq/draw-shape (blob (+ z base) (* base 10) (+ (* base 10))))))

(defn ^:export run-sketch []
  (q/defsketch tunnel-flight
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
