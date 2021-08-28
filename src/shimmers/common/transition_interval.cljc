(ns shimmers.common.transition-interval)

(defprotocol ITransitionInterval
  (complete? [_ t])
  (percent [_ t]))

(defrecord TransitionInterval [base interval]
  ITransitionInterval
  (complete? [_ t]
    (>= t (+ base interval)))
  (percent [_ t]
    (/ (- t base) interval)))

(defn after [t interval]
  (->TransitionInterval t interval))
