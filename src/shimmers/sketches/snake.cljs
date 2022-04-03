(ns shimmers.sketches.snake
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
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

(defn follow [pos {:keys [r] :as target}]
  (tm/mix pos (rp/inside-circle target dr/random) (/ r (* 2 (cq/rel-h 1.0)))))

(defn update-state [{:keys [chain target] :as state}]
  (let [tip (chain/segment-endpoint (last (:segments chain)))]
    (-> (if (g/contains-point? target tip)
          (assoc state :target (gen-target))
          state)
        (update :chain chain/chain-update nil (follow tip target))
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
