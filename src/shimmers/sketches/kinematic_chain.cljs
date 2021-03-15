(ns shimmers.sketches.kinematic-chain
  "Inspired by https://www.youtube.com/watch?v=hbgDqyy8bIwa"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]))

(defrecord KinematicChain [segments])

(defrecord KinematicSegment [base angle length])

(defn segment-follow [{:keys [base length]} target]
  (let [direction (tm/- target base)
        heading (geom/heading direction)]
    (->KinematicSegment (tm/- target (geom/as-cartesian (gv/vec2 length heading)))
                        heading
                        length)))

(defn segment-endpoint [{:keys [base angle length]}]
  (tm/+ base (geom/as-cartesian (gv/vec2 length angle))))

(defn make-chain [start n length]
  (->KinematicChain (repeatedly n #(->KinematicSegment start 0 length))))

(defn chain-follow [{:keys [segments] :as chain} target]
  (loop [segments (reverse segments) target target new-chain []]
    (if (empty? segments)
      (assoc chain :segments (reverse new-chain))
      (let [segment (segment-follow (first segments) target)]
        (recur (rest segments) (:base segment) (conj new-chain segment))))))

(defn chain-propagate [{:keys [segments] :as chain} base]
  (loop [segments segments base base new-chain []]
    (if (empty? segments)
      (assoc chain :segments new-chain)
      (let [s (assoc (first segments) :base base)]
        (recur (rest segments) (segment-endpoint s) (conj new-chain s))))))

(defn chain-update [chain base target]
  (cond-> chain
    target (chain-follow target)
    base (chain-propagate base)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:chains [(assoc (make-chain (gv/vec2 (* (q/width) 0.5) 0)
                               80 4)
                   :color [0.3 0.5 0.5 0.025])
            (assoc (make-chain (gv/vec2 (* (q/width) 0.5) (* (q/height) 0.5))
                               80 4)
                   :color [0.6 0.5 0.5 0.025])
            (assoc (make-chain (gv/vec2 (* (q/width) 0.5) (q/height))
                               80 4)
                   :color [0.9 0.5 0.5 0.025])]})

(defn draw-chain [{:keys [segments]}]
  (q/begin-shape)
  (doseq [s segments]
    (apply q/vertex (:base s)))
  (apply q/vertex (segment-endpoint (last segments)))
  (q/end-shape))

(defn screen-point [w h]
  (gv/vec2 (cq/rel-w w) (cq/rel-h h)))

(defn mouse-target []
  (gv/vec2 (q/mouse-x) (q/mouse-y)))

(defn noise-target [rate bw bh]
  (let [fc (q/frame-count)]
    (gv/vec2 (cq/rel-w (q/noise bw (/ fc rate)))
             (cq/rel-h (q/noise bh (/ fc rate))))))

(defn circle-target [r]
  (let [fc (/ (q/frame-count) 100)
        adjusted-r (+ (* 50 (- (q/noise r fc) 0.5)) r)]
    (geom/translate (geom/as-cartesian (gv/vec2 adjusted-r fc))
                    (screen-point 0.5 0.5))))

(defn update-state [{:keys [chains] :as state}]
  (cond-> state
    true (assoc :chains
                (map-indexed (fn [idx chain]
                               (chain-update
                                chain
                                nil ;; (gv/vec2 (/ (q/width) 2) (q/height))
                                (circle-target (* (inc idx) (cq/rel-h 0.15)))))
                             chains))
    (p/chance 0.001) (update :chains shuffle)))

(defn draw [{:keys [chains]}]
  (q/no-fill)
  ;; (q/background 255)
  (doseq [chain chains]
    (apply q/stroke (:color chain))
    (draw-chain chain)))

(defn ^:export run-sketch []
  (q/defsketch kinematic-chain
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
