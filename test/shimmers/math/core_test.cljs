(ns shimmers.math.core-test
  (:require [shimmers.math.core :as sut]
            [cljs.test :as t :include-macros true
             :refer-macros [deftest is run-tests testing]]
            [thi.ng.math.core :as tm]))

(deftest mod-mixing
  (testing "normal mixing"
    (is (tm/delta= 0.2 (sut/mix-mod 0.1 0.3 0.5)))
    (is (tm/delta= 0.1 (sut/mix-mod 0.1 0.3 0)))
    (is (tm/delta= 0.3 (sut/mix-mod 0.1 0.3 1.0))))

  (testing "boundary mix"
    (is (tm/delta= 0.25 (sut/mix-mod 0 0.5 0.5)))
    (is (tm/delta= 0.755 (sut/mix-mod 0 0.51 0.5)))
    (is (tm/delta= 0.951 (sut/mix-mod 0 0.51 0.1)))
    (is (tm/delta= 0.26 (sut/mix-mod 0.01 0.51 0.5))))

  (testing "normal mixing inverted"
    (is (tm/delta= 0.2 (sut/mix-mod 0.3 0.1 0.5)))
    (is (tm/delta= 0.3 (sut/mix-mod 0.3 0.1 0)))
    (is (tm/delta= 0.1 (sut/mix-mod 0.3 0.1 1.0))))

  (testing "modular mixing"
    (is (tm/delta= 0 (sut/mix-mod 0.1 0.9 0.5)))
    (is (tm/delta= 0.02 (sut/mix-mod 0.1 0.9 0.4)))
    (is (tm/delta= 0.98 (sut/mix-mod 0.1 0.9 0.6)))
    (is (tm/delta= 0.1 (sut/mix-mod 0.1 0.9 0.0)))
    (is (tm/delta= 0.9 (sut/mix-mod 0.1 0.9 1.0))))

  (testing "modular mixing inverted"
    (is (tm/delta= 0 (sut/mix-mod 0.9 0.1 0.5)))
    (is (tm/delta= 0.98 (sut/mix-mod 0.9 0.1 0.4)))
    (is (tm/delta= 0.02 (sut/mix-mod 0.9 0.1 0.6)))
    (is (tm/delta= 0.9 (sut/mix-mod 0.9 0.1 0.0)))
    (is (tm/delta= 0.1 (sut/mix-mod 0.9 0.1 1.0))))

  (testing "modulus 360"
    (is (tm/delta= 0 (sut/mix-mod 0 359 0 360)))
    (is (tm/delta= 359 (sut/mix-mod 0 359 1.0 360)))
    (is (tm/delta= 359.5 (sut/mix-mod 0 359 0.5 360)))
    (is (tm/delta= 90 (sut/mix-mod 0 180 0.5 360)))))

(deftest radians-between
  (is (sut/radians-between? 0 1.0 0.5))
  (is (not (sut/radians-between? 0 0.4 0.5)))
  (is (sut/radians-between? -0.4 0.4 6))
  (is (not (sut/radians-between? -0.4 0.4 0.5)))
  (is (sut/radians-between? 5.5 0.5 0.2))
  (is (not (sut/radians-between? 5.5 0.5 0.6)))
  (is (sut/radians-between? Math/PI 1 0))
  (is (not (sut/radians-between? 1 Math/PI 0))))

(comment (run-tests))
