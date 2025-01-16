(ns shimmers.model.shift
  "Shift over time between different parameter sets. Either by converging all in
  tandem or one parameter at a time."
  (:require [thi.ng.math.core :as tm]))

;; TODO: state machine to schedule a change on one or more parameters in the set
;; and on complettion initiate a new parameter change.

(defn linear [value target base-t complete-t]
  (fn [t]
    (tm/mix* value target
             (tm/clamp (/ (- t base-t)
                          (- complete-t base-t))
                       0.0 1.0))))

(defprotocol IConverge
  (converge [_ t]))

(defrecord Parameter [id domain value update-f]
  IConverge
  (converge [param t]
    (let [value' (update-f t)
          [l h] domain]
      (assoc param :value
             (tm/clamp value' l h)))))

(comment
  (let [param (Parameter. :a [0.0 1.0] 0.0
                          (linear 0.0 1.0 0.5 1.25))]
    (for [t (range 0.0 2.0 0.25)]
      (converge param t))))

