(ns shimmers.sketches.kinematic-chain
  "Inspired by https://www.youtube.com/watch?v=hbgDqyy8bIwa"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

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
  (->KinematicChain [(->KinematicSegment start 0 length)]))

(defn update-chain [{:keys [segments] :as chain} target]
  (loop [segments (reverse segments) target target new-chain []]
    (if (empty? segments)
      (assoc chain :segments (reverse new-chain))
      (let [segment (segment-follow (first segments) target)]
        (recur (rest segments) (:base segment) (conj new-chain segment))))))

(defn setup []
  {:chain (make-chain (gv/vec2 (* (q/width) 0.5) (* (q/height) 0.5))
                      1
                      100)})

(defn draw-chain [{:keys [segments]}]
  (q/begin-shape)
  (doseq [s segments]
    (apply q/vertex (:base s)))
  (apply q/vertex (segment-endpoint (last segments)))
  (q/end-shape))

(defn mouse-target []
  (gv/vec2 (q/mouse-x) (q/mouse-y)))

(defn update-state [state]
  (update state :chain update-chain (mouse-target)))

(defn draw [{:keys [chain]}]
  (q/background 255)
  (draw-chain chain))

(defn ^:export run-sketch []
  (q/defsketch kinematic-chain
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
