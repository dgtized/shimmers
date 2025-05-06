(ns shimmers.algorithm.kinematic-chain
  (:require
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn segment-endpoint [{:keys [base angle length]}]
  (v/+polar base length angle))

(defprotocol IKinematicChain
  (follow [_ target])
  (propagate [_ base]))

(defrecord KinematicSegment [base angle length]
  IKinematicChain
  (follow [_ target]
    (let [direction (tm/- target base)
          heading (g/heading direction)]
      (KinematicSegment. (v/-polar target length heading)
                         heading
                         length)))
  (propagate [link base]
    (assoc link :base base)))

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
        (let [link (propagate (first links) base)]
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

(defn links->chain [start links]
  (->KinematicChain
   (reduce (fn [chain [angle len]]
             (conj chain
                   (if (seq chain)
                     (let [p (segment-endpoint (last chain))]
                       (->KinematicSegment p angle len))
                     (->KinematicSegment start angle len))))
           []
           links)))

(comment
  (links->chain
   (gv/vec2)
   [[0 1] [(/ eq/TAU 4) 1] [0 2] [(* (/ 3 4) eq/TAU) 1] [(* (/ 1 2) eq/TAU) 1] [0 1]]))

(defn chain-update [chain base target]
  (cond-> chain
    target (follow target)
    base (propagate base)))
