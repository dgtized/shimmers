(ns shimmers.math.geometry.triangle-test
  (:require [shimmers.math.geometry.triangle :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :refer [deftest is] :include-macros true])
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

(comment (t/run-tests))
