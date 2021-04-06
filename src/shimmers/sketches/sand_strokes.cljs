(ns shimmers.sketches.sand-strokes
  "Attempting the sand-stroke technique from
  http://www.complexification.net/gallery/machines/sandstroke/ and further
  explained in https://inconvergent.net/2017/grains-of-sand/."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]))

(defn displacement-noise [t]
  (cq/rel-h (* 0.15 (q/noise t 100))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-detail 6 0.75)
  {:t 0
   :density 100
   :shape (gl/line2 (cq/rel-pos 0.1 0.5)
                    (cq/rel-pos 0.9 0.5))
   :displacement (displacement-noise 0)
   :color [0 0.5 0.5 0.05]})

(defn update-state [{:keys [t] :as state}]
  (let [t (mod (+ t (* 0.0025 (rand))) 1.0)]
    (assoc state
           :t t
           :displacement (displacement-noise t))))

(defn draw [{:keys [t density shape displacement color]}]
  (q/stroke-weight 0.5)
  (apply q/stroke color)
  (let [[x y] (geom/point-at shape t)
        line (gl/line2 x (- y displacement) x (+ y displacement))]
    (dotimes [_ density]
      (apply q/point (geom/random-point line)))))

(defn ^:export run-sketch []
  (q/defsketch sand-strokes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
