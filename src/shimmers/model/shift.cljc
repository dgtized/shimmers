(ns shimmers.model.shift
  "Shift over time between different parameter sets. Either by converging all in
  tandem or one parameter at a time."
  (:require [thi.ng.math.core :as tm]))

;; TODO: state machine to schedule a change on one or more parameters in the set
;; and on complettion initiate a new parameter change.

(defprotocol IConverge
  (converge [_ t])
  (converged? [_ t]))

(defrecord Constant [base-t complete-t value]
  IConverge
  (converge [_ _t] value)
  (converged? [_ t]
    (> t complete-t)))

(defrecord Linear [base-t complete-t value target]
  IConverge
  (converge [_ t]
    (tm/mix* value target
             (/ (- t base-t)
                (- complete-t base-t))))
  (converged? [_ t]
    (> t complete-t)))

;; TODO: this is should be generic for vector, num, float, etc
(defrecord Parameter [id domain value updaters]
  IConverge
  (converge [param t]
    (if-let [updater (peek updaters)]
      (if (converged? updater t)
        (update param :updaters pop)
        (let [value' (converge updater t)
              [l h] domain]
          (assoc param :value (tm/clamp value' l h))))
      param))
  (converged? [_ t]
    (every? (fn [updater] (converged? updater t)) updaters)))

(defn make-parameter [id domain value]
  (Parameter. id domain value #queue []))

(defrecord ParameterSet [parameters]
  IConverge
  (converge [pset t]
    (update pset :parameters
            (partial mapv (fn [param] (converge param t)))))
  (converged? [_ t]
    (every? (fn [param] (converged? param t))
            parameters)))

(defn add-updater
  [param updater]
  (update param :updaters conj updater))

(defn add-converge
  [param-set [id updater]]
  (update param-set :parameters
          (partial mapv (fn [param]
                          (if (= id (:id param))
                            (add-updater param updater)
                            param)))))

(comment
  (let [param (reduce add-updater (make-parameter :a [0.0 1.0] 0.0)
                      [(Linear. 1.5 2.0 1.0 0.0)
                       (Linear. 0.5 1.25 0.0 1.0)])]
    (reductions (fn [param t]
                  (converge param t))
                param
                (range 0.0 3.0 0.2)))

  (let [params (reduce add-converge
                       (ParameterSet.
                        [(make-parameter :a [0.0 1.0] 0.0)
                         (make-parameter :b [0.0 1.0] 1.0)])
                       {:a (Linear. 0.5 1.25 0.0 1.0)
                        :b (Linear. 1.5 2.0 1.0 0.0)})]
    (reductions (fn [pset t]
                  (converge pset t))
                params
                (range 0.0 4.0 0.2))))
