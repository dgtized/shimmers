(ns shimmers.model.shift
  "Shift over time between different parameter sets. Either by converging all in
  tandem or one parameter at a time."
  (:require [thi.ng.math.core :as tm]))

;; TODO: state machine to schedule a change on one or more parameters in the set
;; and on complettion initiate a new parameter change.

(defprotocol IUpdateStep
  (step [_ t])
  (done? [_ t]))

(defrecord Constant [base-t complete-t value]
  IUpdateStep
  (step [_ _t] value)
  (done? [_ t]
    (> t complete-t)))

(defrecord Linear [base-t complete-t value target]
  IUpdateStep
  (step [_ t]
    (tm/mix* value target
             (/ (- t base-t)
                (- complete-t base-t))))
  (done? [_ t]
    (> t complete-t)))

(defprotocol IConverge
  (converge [_ t]))

(defrecord Parameter [id domain value updaters]
  IConverge
  (converge [param t]
    (if-let [updater (peek updaters)]
      (if (done? updater t)
        (update param :updaters pop)
        (let [value' (step updater t)
              [l h] domain]
          (assoc param :value (tm/clamp value' l h))))
      param)))

(comment
  (let [param (Parameter. :a [0.0 1.0] 0.0
                          [(Linear. 1.5 2.0 1.0 0.0)
                           (Linear. 0.5 1.25 0.0 1.0)])]
    (reductions (fn [param t]
                  (converge param t))
                param
                (range 0.0 3.0 0.2))))
