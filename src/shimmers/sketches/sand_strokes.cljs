(ns shimmers.sketches.sand-strokes
  "Attempting the sand-stroke technique from
  http://www.complexification.net/gallery/machines/sandstroke/ and further
  explained in https://inconvergent.net/2017/grains-of-sand/."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [kixi.stats.distribution :as ksd]))

(defn displacement-noise [t v]
  (cq/rel-h (* 0.3 (q/noise (/ t 2) v))))

(defn rand-color []
  [(rand-nth [0 0.25 0.4 0.8]) 0.5 0.5 0.2])

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-detail 6 0.75)
  {:t 0
   :v 0
   :density 20
   :shape (gl/line2 (cq/rel-pos 0.05 0.5)
                    (cq/rel-pos 0.95 0.5))
   :displacement (displacement-noise 0 0)
   :color (rand-color)})

(defn update-state [{:keys [t v color] :as state}]
  (let [t' (mod (+ t (* 0.001 (rand))) 1.0)
        new-pass (< t' t)
        v' (if new-pass (inc v) v)
        color' (if new-pass (rand-color) color)]
    (assoc state
           :t t'
           :v v'
           :color color'
           :displacement (displacement-noise t' v'))))

(defn draw [{:keys [t density shape displacement color]}]
  (apply q/stroke color)
  (let [[x y] (geom/point-at shape t)
        line (gl/line2 x (- y displacement) x (+ y displacement))]
    (doseq [p (ksd/sample density (ksd/normal {:mu 0.5 :sd 0.2}))]
      (apply q/point (geom/point-at line p)))))

(defn ^:export run-sketch []
  (q/defsketch sand-strokes
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
