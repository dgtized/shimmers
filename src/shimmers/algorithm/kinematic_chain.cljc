(ns shimmers.algorithm.kinematic-chain
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn project [angle length]
  (g/as-cartesian (gv/vec2 length angle)))

(defn segment-endpoint [{:keys [base angle length]}]
  (tm/+ base (project angle length)))

(defprotocol IKinematicChain
  (follow [_ target])
  (propagate [_ base]))

(defrecord KinematicSegment [base angle length]
  IKinematicChain
  (follow [_ target]
    (let [direction (tm/- target base)
          heading (g/heading direction)]
      (KinematicSegment. (tm/- target (project heading length))
                         heading
                         length))))

(defrecord KinematicChain [segments]
  IKinematicChain
  (follow [chain target]
    (loop [links (reverse segments) target target new-chain []]
      (if (empty? links)
        (assoc chain :segments (reverse new-chain))
        (let [link (follow (first links) target)]
          (recur (rest links) (:base link) (conj new-chain link))))))

  (propagate [chain base]
    (loop [links segments base base new-chain []]
      (if (empty? links)
        (assoc chain :segments new-chain)
        (let [link (assoc (first links) :base base)]
          (recur (rest links) (segment-endpoint link) (conj new-chain link))))))

  g/IVertexAccess
  (vertices [_]
    (conj (mapv :base segments)
          (segment-endpoint (last segments))))

  g/IEdgeAccess
  (edges [_]
    (partition 2 1 (g/vertices _))))

(defn make-chain [start n length]
  (->KinematicChain (repeatedly n #(->KinematicSegment start 0 length))))

(defn chain-update [chain base target]
  (cond-> chain
    target (follow target)
    base (propagate base)))
