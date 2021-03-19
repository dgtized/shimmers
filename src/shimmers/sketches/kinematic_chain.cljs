(ns shimmers.sketches.kinematic-chain
  "Inspired by https://www.youtube.com/watch?v=hbgDqyy8bIwa"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.kinematic-chain :as chain]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]))

(defn screen-point [w h]
  (gv/vec2 (cq/rel-w w) (cq/rel-h h)))

(defn mouse-target []
  (gv/vec2 (q/mouse-x) (q/mouse-y)))

(defn noise-target [rate bw bh]
  (let [fc (q/frame-count)]
    (screen-point (q/noise bw (/ fc rate))
                  (q/noise bh (/ fc rate)))))

(defn circle-target [center r]
  (let [fc (/ (q/frame-count) 100)
        adjusted-r (+ (* 50 (- (q/noise r (* 2 fc)) 0.5)) r)]
    (geom/translate (geom/as-cartesian (gv/vec2 adjusted-r fc))
                    center)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:chains [(assoc (chain/make-chain (screen-point 0.5 0) 80 4)
                   :color [0.35 0.5 0.5 0.025])
            (assoc (chain/make-chain (screen-point 0.5 0.5) 80 4)
                   :color [0.65 0.5 0.5 0.025])
            (assoc (chain/make-chain (screen-point 0.5 1.0) 80 4)
                   :color [0.95 0.5 0.5 0.025])]})

(defn draw-chain [{:keys [segments]}]
  (q/begin-shape)
  (doseq [s segments]
    (apply q/vertex (:base s)))
  (apply q/vertex (chain/segment-endpoint (last segments)))
  (q/end-shape))

(defn update-state [{:keys [chains] :as state}]
  (assoc state :chains
         (map-indexed (fn [idx chain]
                        (chain/chain-update
                         chain
                         (screen-point (+ 0.15 (* 0.6 (/ idx 2))) 0.5)
                         (circle-target (screen-point (/ idx 2) 0.5)
                                        (* (* (inc idx) 0.8) (cq/rel-h 0.2)))))
                      chains)))

(defn draw [{:keys [chains]}]
  (q/no-fill)
  ;; (q/background 255)
  (doseq [chain chains]
    (apply q/stroke (:color chain))
    (draw-chain chain)))

(defn ^:export run-sketch []
  ;; 20210315
  (q/defsketch kinematic-chain
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
