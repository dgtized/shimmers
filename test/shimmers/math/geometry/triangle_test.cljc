(ns shimmers.math.geometry.triangle-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.geometry.triangle :as sut]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(deftest decomposition
  (let [t (gt/triangle2 [0 0] [0 10] [10 0])]
    (is (= [(gt/triangle2 [0 10] [5 5] [0 0])
            (gt/triangle2 [10 0] [5 5] [0 0])]
           (sut/decompose t {:sample (fn [] 0.5)})))
    (is (= [(gt/triangle2 [0 10] [10 0] [5 5])
            (gt/triangle2 [10 0] [0 0] [5 5])
            (gt/triangle2 [0 0] [0 10] [5 5])]
           (sut/decompose t {:mode :centroid
                             :inner-point (fn [_] (gv/vec2 5 5))})))
    (is (= [(gt/triangle2 [[0 10] [5 5] [0 5]])
            (gt/triangle2 [[10 0] [5 5] [5 0]])
            (gt/triangle2 [0 0] [5 0] [0 5])
            (gt/triangle2 [5 5] [5 0] [0 5])]
           (sut/decompose t {:mode :inset
                             :sample (fn [] 0.5)})))
    (is (= [(gt/triangle2 [[0 10] [2 8] [0 0]])
            (gt/triangle2 [[10 0] [8 2] [0 0]])
            (gt/triangle2 [2 8] [8 2] [0 0])]
           (sut/decompose t {:mode :trisect
                             :sample-low (fn [] 0.2)
                             :sample-high (fn [] 0.8)})))))

(deftest area
  (let [t (gt/triangle2 [0 0] [2 0] [0 2])]
    (is (tm/delta= 2 (g/area t)))
    (is (tm/delta= 2 (sut/signed-area t))))
  (let [t (gt/triangle2 [2 0] [0 0] [0 2])]
    (is (tm/delta= 2 (g/area t)))
    (is (tm/delta= -2 (sut/signed-area t)))))

(comment (t/run-tests))
