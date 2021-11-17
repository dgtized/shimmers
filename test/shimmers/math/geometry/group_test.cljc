(ns shimmers.math.geometry.group-test
  (:require
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :as t :include-macros true
             :refer [deftest is]])
   [shimmers.math.geometry.group :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(defn g-circle []
  (sut/group (gc/circle)))

(defn g-rect []
  (sut/group (rect/rect)))

(defn g-rect-circle []
  (sut/group [(rect/rect) (gc/circle)]))

(deftest tiling
  (is (= (sut/group [(gc/circle 5 5 5)])
         (sut/tile-grid (rect/rect 10)
                        (repeatedly 1 g-circle)
                        {:scale 1.0})))
  (is (= (sut/group [(gc/circle 2.5 2.5 2.5)
                     (gc/circle 7.5 2.5 2.5)
                     (gc/circle 2.5 7.5 2.5)
                     (gc/circle 7.5 7.5 2.5)])
         (sut/tile-grid (rect/rect 10)
                        (repeatedly 4 g-circle)
                        {:scale 1.0})))
  (is (= (sut/group [(gc/circle 2.5 2.5 2.5)
                     (gc/circle 7.5 2.5 2.5)
                     (gc/circle 2.5 7.5 2.5)])
         (sut/tile-grid (rect/rect 10)
                        (repeatedly 3 g-circle)
                        {:scale 1.0})))
  (is (= (sut/group [(gc/circle 2.5 2.5 2.5)
                     (gc/circle 7.5 2.5 2.5)
                     (gc/circle 2.5 7.5 2.5)
                     (gc/circle 7.5 7.5 2.5)])
         (sut/tile-grid (rect/rect 10)
                        (repeatedly 5 g-circle)
                        {:scale 1.0 :rows 2 :cols 2}))
      "ignores extra elements that won't fit in specified grid"))

(deftest centering
  (is (= (gv/vec2) (g/centroid (g-circle))))
  (is (= (gv/vec2 0.5 0.5) (g/centroid (g-rect))))
  (is (= (gv/vec2 0.25 0.25) (g/centroid (g-rect-circle)))))

(deftest grid-fit
  (is (= [1 1 0] (sut/fit-grid 1 {})))
  (is (= [2 1 0] (sut/fit-grid 2 {})))
  (is (= [2 2 1] (sut/fit-grid 3 {})))
  (is (= [2 2 0] (sut/fit-grid 4 {})))
  (is (= [3 3 0] (sut/fit-grid 9 {})))

  (is (= [5 2 0] (sut/fit-grid 10 {:rows 2})))
  (is (= [2 5 0] (sut/fit-grid 10 {:cols 2})))
  (is (= [3 3 -1] (sut/fit-grid 10 {:rows 3 :cols 3})))
  )

(comment (t/run-tests))
