(ns shimmers.math.geometry.ellipse-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest is]]
      :cljs [cljs.test :as t :include-macros true
             :refer [deftest is]])
   [shimmers.math.geometry.ellipse :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as v]
   [thi.ng.math.core :as m :refer [TWO_PI]]))

(deftest basics
  (is (m/delta= TWO_PI (g/area (sut/ellipse 2 1))))

  (is (= (rect/rect -3 -2 6 4) (g/bounds (sut/ellipse 3 2))))
  (is (= 6 (g/width (sut/ellipse 3 1))))
  (is (= 4 (g/height (sut/ellipse 3 2))))

  (is (= (gc/circle 3) (g/bounding-circle (sut/ellipse 3 2))))

  (is (= (sut/ellipse 2 1) (g/center (sut/ellipse [1 1] 2 1))))
  (is (= (sut/ellipse [2 2] 1 2) (g/center (sut/ellipse [1 1] 1 2) [2 2])))
  (is (= (v/vec2 1 1) (g/centroid (sut/ellipse [1 1] 1 2)))))

(comment (t/run-tests))
