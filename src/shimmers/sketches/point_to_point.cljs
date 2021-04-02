(ns shimmers.sketches.point-to-point
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.common.sequence :as cs]
            [thi.ng.geom.core :as geom]))

;; Some experiments with drawing lines derived from a set of random points

(defn short-pairs [points]
  (->> (for [[u v] (cs/all-pairs points)]
         [(geom/dist u v) [u v]])
       (sort-by first)
       (map second)))

(defn setup []
  {:points (geometry/generate-points 64 #(+ 0.5 (* 0.15 (q/random-gaussian))))})

(defn update-state [state]
  state)

(defn draw [{:keys [points]}]
  (q/background 255)
  (q/stroke-weight 2)
  (q/ellipse-mode :radius)
  (q/stroke 0 0 0)
  (doseq [point points
          :let [[x y] (cq/rel-pos point)]]
    (q/ellipse x y 0.2 0.2))

  (q/stroke-weight 0.25)
  (doseq [[p q] (take (* 0.5 (count points)) (short-pairs points))]
    (q/line (cq/rel-pos p) (cq/rel-pos q)))

  (doseq [point points
          :let [[x y] (cq/rel-pos point)]]
    (q/line 0 y (q/width) y)
    (q/line x 0 x (q/height)))
  )

(defn ^:export run-sketch []
  (q/defsketch point-to-point
    :host "quil-host"
    :size [600 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
