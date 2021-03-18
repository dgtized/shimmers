(ns shimmers.math.geometry-test
  (:require [cljs.test :as t :include-macros true
             :refer-macros [deftest is run-tests]]
            [shimmers.math.geometry :as sut]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]))

(deftest decomposition
  (let [t (gt/triangle2 [0 0] [0 1] [1 0])]
    (is (= [(gt/triangle2 [0 1] [0.5 0.5] [0 0])
            (gt/triangle2 [1 0] [0.5 0.5] [0 0])]
           (sut/decompose t {:sample (fn [] 0.5)})))
    (is (= [(gt/triangle2 [0 1] [1 0] [0.5 0.5])
            (gt/triangle2 [1 0] [0 0] [0.5 0.5])
            (gt/triangle2 [0 0] [0 1] [0.5 0.5])]
           (sut/decompose t {:mode :centroid
                             :inner-point (fn [_] (gv/vec2 0.5 0.5))})))
    (is (= [(gt/triangle2 [[0 1] [0.5 0.5] [0 0.5]])
            (gt/triangle2 [[1 0] [0.5 0.5] [0.5 0]])
            (gt/triangle2 [0 0] [0.5 0] [0 0.5])
            (gt/triangle2 [0.5 0.5] [0.5 0] [0 0.5])]
           (sut/decompose t {:mode :inset
                             :sample (fn [] 0.5)})))
    (is (= [(gt/triangle2 [[0 1] [0.2 0.8] [0 0]])
            (gt/triangle2 [[1 0] [0.8 0.2] [0 0]])
            (gt/triangle2 [0.2 0.8] [0.8 0.2] [0 0])]
           (sut/decompose t {:mode :trisect
                             :sample-low (fn [] 0.2)
                             :sample-high (fn [] 0.8)})))))

(comment (run-tests))
