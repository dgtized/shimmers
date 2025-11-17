(ns shimmers.math.stair
  (:require
   [clojure.math :as math]
   [shimmers.math.equations :as eq]
   [thi.ng.math.core :as tm]))

(defn stairs ^double [^double k ^double x]
  (- x (/ (math/sin (* x eq/TAU k)) (* 8 k))))

;; If k is negative should it be forced positive with abs or fail as a precondition?
(defn staircase
  "Density function to subdivide `x` in [0,1] from a linear distribution to `k`
  higher density regions."
  ^double [^double k ^double x]
  (let [base (tm/floor k)]
    (cond
      (< k 1.0)
      (tm/mix* x (stairs 1.0 x) k)
      (tm/delta= k base)
      (stairs k x)
      :else
      (tm/mix* (stairs base x) (stairs (+ 1 base) x) (tm/fract k)))))
