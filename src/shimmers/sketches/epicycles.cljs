(ns shimmers.sketches.epicycles
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn rotate-chainlinks [chain base dt]
  (-> chain
      (update :segments
              (fn [s] (map-indexed (fn [i segment]
                                    (update segment :angle +
                                            (* (if (odd? i)
                                                 -1
                                                 1)
                                               (/ (inc i) 10) (* (inc i) dt))))
                                  s)))
      (chain/propagate base)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n 6
        length (cq/rel-h (/ 0.45 n))]
    {:chain (chain/->KinematicChain
             (for [i (range n)]
               (chain/->KinematicSegment (tm/+ (gv/vec2) (tm/* (gv/vec2 length 0) i))
                                         0
                                         length)))}))

(defn update-state [state]
  (update state :chain rotate-chainlinks (gv/vec2) 0.02))

(defn draw [{:keys [chain]}]
  ;; (println chain)
  (q/background 1.0)
  (q/with-translation (cq/rel-vec 0.5 0.5)
    (doseq [[p q] (g/edges chain)]
      (q/line p q))
    (doseq [p (g/vertices chain)]
      (cq/circle p 5))))

(sketch/defquil epicycles
  :created-at "2022-12-06"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
