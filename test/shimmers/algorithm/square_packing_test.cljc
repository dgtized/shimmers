(ns shimmers.algorithm.square-packing-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.algorithm.square-packing :as sut]
   [thi.ng.geom.rect :as rect]))

(def rectangle (rect/rect 0 0 100 120))

(deftest splitting-panes
  (is (= [(rect/rect 25 35 50 50)
          (rect/rect 0 0 100 35)
          (rect/rect 0 85 100 35)
          (rect/rect 0 35 25 50)
          (rect/rect 75 35 25 50)]
         (sut/split-panes rectangle 50 [0.5 0.5] :row))
      "row major")
  (is (= [(rect/rect 25 35 50 50)
          (rect/rect 25 0 50 35)
          (rect/rect 25 85 50 35)
          (rect/rect 0 0 25 120)
          (rect/rect 75 0 25 120)]
         (sut/split-panes rectangle 50 [0.5 0.5] :column))
      "column major")
  (is (= [(rect/rect 25 35 50 50)
          (rect/rect 75 35 25 85)
          (rect/rect 0 85 75 35)
          (rect/rect 0 0 25 85)
          (rect/rect 25 0 75 35)]
         (sut/split-panes rectangle 50 [0.5 0.5] :clockwise))
      "clockwise")
  (is (= [(rect/rect 25 35 50 50)
          (rect/rect 75 0 25 85)
          (rect/rect 0 0 75 35)
          (rect/rect 0 35 25 85)
          (rect/rect 25 85 75 35)]
         (sut/split-panes rectangle 50 [0.5 0.5] :counter-clockwise))
      "counter clockwise")
  (is (= [(rect/rect 25 35 50 50) ;; re-order?
          ;; top row
          (rect/rect 0 0 25 35)
          (rect/rect 25 0 50 35)
          (rect/rect 75 0 25 35)
          ;; middle row without inner rectangle
          (rect/rect 0 35 25 50)
          (rect/rect 75 35 25 50)
          ;; bottom row
          (rect/rect 0 85 25 35)
          (rect/rect 25 85 50 35)
          (rect/rect 75 85 25 35)]
         (sut/split-panes rectangle 50 [0.5 0.5] :all))
      "all"))

(deftest alignment
  (t/testing "east"
    (is (= (rect/rect 10 0 20 20)
           (sut/align-to :right (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect 15 0 20 20)
           (sut/align-to :right 5 (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect 10 0 20 20)
           (sut/align-to :right (rect/rect 10) (rect/rect -10 0 20))))
    (is (= (rect/rect 10 0 20 20)
           (sut/align-to :right (rect/rect 10) (rect/rect 10 0 20)))))

  (t/testing "west"
    (is (= (rect/rect -20 0 20 20)
           (sut/align-to :left (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect -25 0 20 20)
           (sut/align-to :left 5 (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect -20 0 20 20)
           (sut/align-to :left (rect/rect 10) (rect/rect -10 0 20))))
    (is (= (rect/rect -20 0 20 20)
           (sut/align-to :left (rect/rect 10) (rect/rect 10 0 20)))))

  (t/testing "north"
    (is (= (rect/rect 0 -20 20 20)
           (sut/align-to :top (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect 0 -25 20 20)
           (sut/align-to :top 5 (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect 0 -20 20 20)
           (sut/align-to :top (rect/rect 10) (rect/rect 0 -10 20))))
    (is (= (rect/rect 0 -20 20 20)
           (sut/align-to :top (rect/rect 10) (rect/rect 0 10 20)))))

  (t/testing "south"
    (is (= (rect/rect 0 10 20 20)
           (sut/align-to :bottom (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect 0 15 20 20)
           (sut/align-to :bottom 5 (rect/rect 10) (rect/rect 0 0 20))))
    (is (= (rect/rect 0 10 20 20)
           (sut/align-to :bottom (rect/rect 10) (rect/rect 0 -10 20))))
    (is (= (rect/rect 0 10 20 20)
           (sut/align-to :bottom (rect/rect 10) (rect/rect 0 10 20))))))

(comment (t/run-tests))
