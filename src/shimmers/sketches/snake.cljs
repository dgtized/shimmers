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
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn gen-target []
  (gc/circle (cq/rel-vec (dr/random) (dr/random))
             (dr/random (cq/rel-h 0.05) (cq/rel-h 0.2))))

(defn gen-segment [{:keys [angle] :as segment}]
  (let [base (chain/segment-endpoint segment)]
    (chain/->KinematicSegment base (dr/gaussian angle 0.5)
                              (Math/abs (dr/gaussian 18 5)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0
   :target (gen-target)
   :chain (->> (chain/->KinematicSegment (cq/rel-vec (dr/random) (dr/random))
                                         (dr/random eq/TAU) 8)
               (iterate gen-segment)
               (take 32)
               chain/->KinematicChain)})

(defn follow [pos {:keys [p r]} t]
  (let [[x y] (tm/* pos 0.01)
        n (q/noise x y t)]
    (tm/mix pos (tm/+ p (v/polar (* 1.8 r n) t))
            (* 0.03 n))))

(defn update-state [{:keys [chain target t] :as state}]
  (let [tip (chain/segment-endpoint (last (:segments chain)))]
    (-> (if (g/contains-point? target tip)
          (assoc state :target (gen-target))
          state)
        (update :chain chain/chain-update nil (follow tip target t))
        (update :t + 0.01))))

(defn draw [{:keys [chain]}]
  (q/background 1.0 0.001)
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
