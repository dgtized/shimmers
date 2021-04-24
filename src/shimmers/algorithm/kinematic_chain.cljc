(ns shimmers.algorithm.kinematic-chain
  (:require [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn- project [angle length]
  (geom/as-cartesian (gv/vec2 length angle)))

(defn- segment-endpoint [{:keys [base angle length]}]
  (tm/+ base (project angle length)))

(defprotocol IKinematicChain
  (follow [_ target])
  (propagate [_ base]))

(defrecord KinematicSegment [base angle length]
  IKinematicChain
  (follow [_ target]
    (let [direction (tm/- target base)
          heading (geom/heading direction)]
      (KinematicSegment. (tm/- target (project heading length))
                         heading
                         length))))

(defrecord KinematicChain [segments]
  IKinematicChain
  (follow [chain target]
    (loop [segments (reverse segments) target target new-chain []]
      (if (empty? segments)
        (assoc chain :segments (reverse new-chain))
        (let [segment (follow (first segments) target)]
          (recur (rest segments) (:base segment) (conj new-chain segment))))))

  (propagate [chain base]
    (loop [segments segments base base new-chain []]
      (if (empty? segments)
        (assoc chain :segments new-chain)
        (let [s (assoc (first segments) :base base)]
          (recur (rest segments) (segment-endpoint s) (conj new-chain s))))))

  geom/IVertexAccess
  (vertices [_]
    (conj (mapv :base segments)
          (segment-endpoint (last segments))))

  geom/IEdgeAccess
  (edges [_]
    (partition 2 1 (geom/vertices _))))

(defn make-chain [start n length]
  (->KinematicChain (repeatedly n #(->KinematicSegment start 0 length))))

(defn chain-update [chain base target]
  (cond-> chain
    target (follow target)
    base (propagate base)))
