(ns shimmers.sketches.kinematic-chain
  "Inspired by https://www.youtube.com/watch?v=hbgDqyy8bIwa"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.kinematic-chain :as chain]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn rel-v [w h]
  (gv/vec2 (cq/rel-pos w h)))

(defn mouse-target []
  (gv/vec2 (q/mouse-x) (q/mouse-y)))

(defn noise-target [rate bw bh]
  (let [fc (q/frame-count)]
    (rel-v (q/noise bw (/ fc rate))
           (q/noise bh (/ fc rate)))))

(defn sin-target []
  (let [t (q/millis)]
    (rel-v (tm/map-interval (q/cos (+ Math/PI (/ t 10000))) [-1 1] [0.05 0.95])
           (tm/map-interval (q/sin (/ t 2000)) [-1 1] [0.1 0.9]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:chain (assoc (chain/make-chain (sin-target) 96 12)
                 :color [0.35 0.6 0.6 0.05])})

(defn draw-chain [{:keys [segments]}]
  (q/begin-shape)
  (doseq [s segments]
    (apply q/vertex (:base s)))
  (apply q/vertex (chain/segment-endpoint (last segments)))
  (q/end-shape))

(defn update-state [state]
  (-> state
      (update :chain chain/chain-update nil (sin-target))
      (update-in [:chain :color 0] (fn [c] (mod (+ 0.001 c) 1.0)))))

(defn draw [{:keys [chain]}]
  (q/no-fill)
  ;; (q/background 255)
  (apply q/stroke (:color chain))
  (draw-chain chain))

(defn ^:export run-sketch []
  ;; 20210319
  (q/defsketch kinematic-chain
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
