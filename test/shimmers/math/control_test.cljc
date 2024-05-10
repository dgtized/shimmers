(ns shimmers.math.control-test
  (:require
   [clojure.math :as math]
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.control :as sut]
   [shimmers.math.equations :as eq]
   [thi.ng.math.core :as tm]))

(deftest angular-delta
  (is (tm/delta= 0.0 (sut/angular-delta 1.0 1.0)))
  (is (tm/delta= 1.0 (sut/angular-delta 0.0 1.0)))
  (is (tm/delta= math/PI (sut/angular-delta 0.0 math/PI)))
  (is (tm/delta= (- (dec math/PI)) (sut/angular-delta 0.0 (inc math/PI))))
  (is (tm/delta= 1.0 (sut/angular-delta 0.0 (inc eq/TAU))))
  (is (tm/delta= -1.0 (sut/angular-delta 0.0 (- (inc eq/TAU)))))
  (is (tm/delta= 1.0 (sut/angular-delta 0.0 (- (dec eq/TAU))))))
