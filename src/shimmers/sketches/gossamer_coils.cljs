(ns shimmers.sketches.gossamer-coils
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.kinematic-chain :as chain]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]))

(defn circle-target [center r]
  (let [fc (/ (q/frame-count) 100)
        adjusted-r (+ (* 50 (- (q/noise r (* 2 fc)) 0.5)) r)]
    (geom/translate (geom/as-cartesian (gv/vec2 adjusted-r fc))
                    center)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:chains [(assoc (chain/make-chain (cq/rel-vec 0.5 0) 80 4)
                   :color [0.35 0.5 0.5 0.025])
            (assoc (chain/make-chain (cq/rel-vec 0.5 0.5) 80 4)
                   :color [0.65 0.5 0.5 0.025])
            (assoc (chain/make-chain (cq/rel-vec 0.5 1.0) 80 4)
                   :color [0.95 0.5 0.5 0.025])]})

(defn draw-chain [chain]
  (q/begin-shape)
  (doseq [[x y] (geom/vertices chain)]
    (q/vertex x y))
  (q/end-shape))

(defn update-state [{:keys [chains] :as state}]
  (assoc state :chains
         (map-indexed (fn [idx chain]
                        (chain/chain-update
                         chain
                         (cq/rel-vec (+ 0.15 (* 0.6 (/ idx 2))) 0.5)
                         (circle-target (cq/rel-vec (/ idx 2) 0.5)
                                        (* (* (inc idx) 0.8) (cq/rel-h 0.2)))))
                      chains)))

(defn draw [{:keys [chains]}]
  (q/no-fill)
  ;; (q/background 255)
  (doseq [chain chains]
    (apply q/stroke (:color chain))
    (draw-chain chain)))

(sketch/defquil gossamer-coils
  :created-at "2021-03-15"
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
