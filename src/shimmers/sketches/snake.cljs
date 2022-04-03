(ns shimmers.sketches.snake
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn gen-segment [{:keys [angle] :as segment}]
  (let [base (chain/segment-endpoint segment)]
    (chain/->KinematicSegment base (dr/gaussian angle 0.5)
                              (Math/abs (dr/gaussian 18 5)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0
   :chain (->> (chain/->KinematicSegment (gv/vec2) (dr/random eq/TAU) 8)
               (iterate gen-segment)
               (take 32)
               chain/->KinematicChain)})

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
