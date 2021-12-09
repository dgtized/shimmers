(ns shimmers.math.geometry.intersection-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true
                      :refer [deftest is]])
            [shimmers.math.geometry.intersection :as sut]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.line :as gl]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.vector :as gv]))

(deftest circle-segment-isec
  (let [p (gv/vec2 (/ (Math/sqrt 2) 2) (/ (Math/sqrt 2) 2))]
    (is (tm/delta= [-1.70710678 -0.2928932 (tm/- p) p]
                   (sut/circle-segment-intersection (gc/circle [1 1] 1) (gl/line2 [1 1] [2 2])))))
  (is (nil? (sut/circle-segment-intersection (gc/circle [1 1] 1) (gl/line2 [1 1] [2 0]))))
  (is (tm/delta= [-0.2 0.2 [-1 0] [1 0]]
                 (sut/circle-segment-intersection (gc/circle [2 0] 1) (gl/line2 [0 0] [5 0]))))
  (is (tm/delta= [-2 0 [-1 0] [1 0]]
                 (sut/circle-segment-intersection (gc/circle [0 0] 1) (gl/line2 [1 0] [2 0]))))
  (is (tm/delta= [-4.872983346207417
                  2.872983346207417
                  [-3.872983455657959 1]
                  [3.872983455657959 1]]
                 (sut/circle-segment-intersection (gc/circle [0 0] 4) (gl/line2 [1 1] [2 1])))))

(comment (t/run-tests))
