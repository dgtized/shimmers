(ns shimmers.math.geometry.arc-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.geometry.arc :as sut]
   [thi.ng.math.core :as tm]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(deftest half-angle
  (is (tm/delta= tm/HALF_PI (sut/half-angle (sut/arc 0 tm/PI))))
  (is (tm/delta= tm/HALF_PI (sut/half-angle (sut/arc (- tm/HALF_PI) tm/HALF_PI))))
  (is (tm/delta= (* 0.4 eq/TAU) (sut/half-angle (sut/arc (* eq/TAU 0.1) (* eq/TAU 0.9)))))
  (is (tm/delta= (* 0.1 eq/TAU) (sut/half-angle (sut/arc (* eq/TAU 0.9) (* eq/TAU 0.1)))))
  (is (tm/delta= (* 0.1 eq/TAU) (sut/half-angle (sut/arc (- (* eq/TAU 0.1)) (* eq/TAU 0.1))))))

(deftest contains-point
  (is (g/contains-point? (sut/arc (- tm/QUARTER_PI) tm/QUARTER_PI) (gv/vec2 0.0 0)))
  (is (g/contains-point? (sut/arc (- tm/QUARTER_PI) tm/QUARTER_PI) (gv/vec2 0.5 0)))
  (is (g/contains-point? (sut/arc (- tm/QUARTER_PI) tm/QUARTER_PI) (gv/vec2 1.0 0)))
  (is (not (g/contains-point? (sut/arc (- tm/QUARTER_PI) tm/QUARTER_PI) (gv/vec2 -0.5 0))))
  (is (not (g/contains-point? (sut/arc (- tm/QUARTER_PI) tm/QUARTER_PI) (gv/vec2 1.5 0))))
  (is (not (g/contains-point? (sut/arc (- tm/QUARTER_PI) tm/QUARTER_PI) (gv/vec2 0 0.5)))))
