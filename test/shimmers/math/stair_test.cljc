(ns shimmers.math.stair-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.stair :as ms]
   [thi.ng.math.core :as tm]))

(deftest staircase
  (is (tm/delta=
       (map (fn [x] (ms/staircase 1.0 x)) (tm/norm-range 10.0))
       [0.0 0.026526843463440863 0.08111793546310582 0.18111793546310578 0.32652684346344085
        0.5 0.6734731565365591 0.8188820645368942 0.9188820645368942 0.9734731565365592 1.0]))
  (is (tm/delta=
       (map (fn [x] (ms/staircase 2.0 x)) (tm/norm-range 16.0))
       [0.0 0.018305826175840784 0.0625 0.14330582617584078 0.25 0.35669417382415924 0.4375 0.48169417382415924
        0.5 0.5183058261758408 0.5625 0.6433058261758408 0.75 0.8566941738241592 0.9375 0.9816941738241592 1.0]))
  (is (tm/delta=
       (map (fn [x] (ms/staircase 4.0 x)) (tm/norm-range 16.0))
       [0.0 0.03125 0.125 0.21875 0.25 0.28125 0.375 0.46875 0.5
        0.53125 0.625 0.71875 0.75 0.78125 0.875 0.96875 1.0])))
