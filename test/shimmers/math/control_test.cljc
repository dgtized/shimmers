(ns shimmers.math.control-test
  (:require [shimmers.math.control :as sut]
            [clojure.test :as t :refer [deftest is] :include-macros true]
            [thi.ng.math.core :as tm]
            [shimmers.math.equations :as eq]))

(deftest angular-delta
  (is (tm/delta= 0.0 (sut/angular-delta 1.0 1.0)))
  (is (tm/delta= 1.0 (sut/angular-delta 0.0 1.0)))
  (is (tm/delta= Math/PI (sut/angular-delta 0.0 Math/PI)))
  (is (tm/delta= (- (dec Math/PI)) (sut/angular-delta 0.0 (inc Math/PI))))
  (is (tm/delta= 1.0 (sut/angular-delta 0.0 (inc eq/TAU))))
  (is (tm/delta= -1.0 (sut/angular-delta 0.0 (- (inc eq/TAU)))))
  (is (tm/delta= 1.0 (sut/angular-delta 0.0 (- (dec eq/TAU))))))
