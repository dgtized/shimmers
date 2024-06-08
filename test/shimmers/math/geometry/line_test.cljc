(ns shimmers.math.geometry.line-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.geometry.line]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(deftest line2-delta=
  (is (tm/delta= (gl/line2 [0 0] [(/ 1 7) (/ 1 3)])
                 (gl/line2 [0 0] [(/ 1 7) 0.333333]))))

(deftest line2-headings-interface
  (is (tm/delta= (g/heading (gl/line2 [0 0] [2 0])) 0))
  (is (tm/delta= (g/heading (gl/line2 [0 0] [0 1])) tm/HALF_PI))
  (is (tm/delta= (g/heading-xy (gl/line2 [0 0] [2 0])) 0))
  (is (tm/delta= (g/heading-xy (gl/line2 [0 0] [0 1])) tm/HALF_PI))
  (is (tm/delta= (g/angle-between (gl/line2 [0 0] [2 1]) [2 1]) 0.0))
  (is (tm/delta= (g/angle-between (gl/line2 [0 0] [1 0]) [0 1]) tm/HALF_PI))
  (is (tm/delta= (g/slope-xy (gl/line2 [0 0] [2 0])) 0))
  (is (tm/delta= (g/slope-xy (gl/line2 [0 0] [1 1])) 1))
  (is (tm/delta= (g/slope-xy (gl/line2 [0 0] [2 1])) 0.5))
  (is (tm/delta= (g/slope-xy (gl/line2 [0 0] [1 2])) 2)))

(deftest line3-delta=
  (is (tm/delta= (gl/line2 [0 0 0] [(/ 1 7) (/ 1 3) 0.666666])
                 (gl/line2 [0 0 0] [(/ 1 7) 0.333333 (/ 2 3)]))))

(deftest line3-headings-interface
  (is (tm/delta= (g/heading (gl/line3 [0 0 0] [2 0 0])) 0))
  (is (tm/delta= (g/heading (gl/line3 [0 0 0] [0 1 0])) tm/HALF_PI))
  (is (tm/delta= (g/heading-xy (gl/line3 [0 0 0] [2 0 0])) 0))
  (is (tm/delta= (g/heading-xy (gl/line3 [0 0 0] [0 1 0])) tm/HALF_PI))
  (is (tm/delta= (g/heading-xz (gl/line3 [0 0 0] [2 0 0])) 0))
  (is (tm/delta= (g/heading-xz (gl/line3 [0 0 0] [0 0 1])) tm/HALF_PI))
  (is (tm/delta= (g/heading-yz (gl/line3 [0 0 0] [0 2 0])) 0))
  (is (tm/delta= (g/heading-yz (gl/line3 [0 0 0] [0 0 1])) tm/HALF_PI))
  (is (tm/delta= (g/angle-between (gl/line3 [0 0 0] [1 0 1]) [1 0 1]) 0.0 1e-3))
  (is (tm/delta= (g/angle-between (gl/line3 [0 0 0] [1 0 0]) [0 1 0]) tm/HALF_PI))
  (is (tm/delta= (g/slope-xy (gl/line3 [0 0 0] [2 0 0])) 0))
  (is (tm/delta= (g/slope-xy (gl/line3 [0 0 0] [1 1 0])) 1))
  (is (tm/delta= (g/slope-xy (gl/line3 [0 0 0] [2 1 0])) 0.5))
  (is (tm/delta= (g/slope-xz (gl/line3 [0 0 0] [1 0 1])) 1))
  (is (tm/delta= (g/slope-xz (gl/line3 [0 0 0] [1 0 2])) 2))
  (is (tm/delta= (g/slope-yz (gl/line3 [0 0 0] [0 1 1])) 1))
  (is (tm/delta= (g/slope-yz (gl/line3 [0 0 0] [0 1 2])) 2)))
