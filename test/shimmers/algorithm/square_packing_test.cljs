(ns shimmers.algorithm.square-packing-test
  (:require [cljs.test :as t :refer-macros [deftest is] :include-macros true]
            [shimmers.algorithm.square-packing :as sut]
            [thi.ng.geom.rect :as rect]))

(def rectangle (rect/rect 0 0 100 100))

(deftest splitting-panes
  (is (= [(rect/rect 25 25 50 50)
          (rect/rect 0 0 100 25)
          (rect/rect 0 75 100 25)
          (rect/rect 0 25 25 50)
          (rect/rect 75 25 25 50)]
         (sut/split-panes rectangle 50 [0.5 0.5] :row)))
  (is (= [(rect/rect 25 25 50 50)
          (rect/rect 25 0 50 25)
          (rect/rect 25 75 50 25)
          (rect/rect 0 0 25 100)
          (rect/rect 75 0 25 100)]
         (sut/split-panes rectangle 50 [0.5 0.5] :column))))

(comment (t/run-tests))
