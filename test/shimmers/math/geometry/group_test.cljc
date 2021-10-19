(ns shimmers.math.geometry.group-test
  (:require [cljs.test :as t :include-macros true
             :refer-macros [deftest is]]
            [shimmers.math.geometry.group :as sut]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.rect :as rect]))

(deftest tiling
  (is (= (sut/group [(gc/circle 5 5 5)])
         (sut/tile-grid (rect/rect 10) (repeatedly 1 #(sut/group (gc/circle))) {:scale 1.0})))
  (is (= (sut/group [(gc/circle 2.5 2.5 2.5)
                     (gc/circle 7.5 2.5 2.5)
                     (gc/circle 2.5 7.5 2.5)
                     (gc/circle 7.5 7.5 2.5)])
         (sut/tile-grid (rect/rect 10) (repeatedly 4 #(sut/group (gc/circle))) {:scale 1.0})))
  (is (= (sut/group [(gc/circle 2.5 2.5 2.5)
                     (gc/circle 7.5 2.5 2.5)
                     (gc/circle 2.5 7.5 2.5)])
         (sut/tile-grid (rect/rect 10) (repeatedly 3 #(sut/group (gc/circle))) {:scale 1.0}))))

(comment (t/run-tests))
