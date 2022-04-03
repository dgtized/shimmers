(ns shimmers.sketches.snake
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.algorithm.kinematic-chain :as chain]
            [shimmers.math.deterministic-random :as dr]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as g]
            [shimmers.math.vector :as v]))

(defn gen-segment []
  (chain/->KinematicSegment (gv/vec2) 0 (Math/abs (dr/gaussian 14 5))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0
   :chain (chain/->KinematicChain (repeatedly 18 #(gen-segment)))})

(defn follow [pos t]
  (let [[x y] (v/polar (cq/rel-h 0.4) t)
        n (q/noise (/ x 100) (/ y 100) t)
        target (v/polar (+ (cq/rel-h 0.05) (* (cq/rel-h 0.6) n)) t)]
    (tm/mix pos target 0.1)))

(defn update-state [{:keys [chain t] :as state}]
  (let [tip (chain/segment-endpoint (last (:segments chain)))]
    (-> state
        (update :chain chain/chain-update nil (follow tip t))
        (update :t + 0.01))))

(defn draw [{:keys [chain]}]
  (q/background 1.0 0.001)
  (apply q/translate (cq/rel-vec 0.5 0.5))
  (q/stroke 0.0 0.05)
  (q/no-fill)
  (let [vertices (g/vertices chain)]
    (cq/draw-path vertices)))

(sketch/defquil snake
  :created-at "2022-04-02"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
