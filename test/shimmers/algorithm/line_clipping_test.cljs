(ns shimmers.algorithm.line-clipping-test
  (:require [cljs.test :as t :refer-macros [deftest is] :include-macros true]
            [shimmers.algorithm.line-clipping :as sut]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Test helper

(defn line-delta=
  ([l1 l2] (line-delta= l1 l2 1e-4))
  ([{[p1 q1] :points} {[p2 q2] :points} eps]
   (and (tm/delta= p1 p2 eps) (tm/delta= q1 q2 eps))))

;; Body of tests

(def r (rect/rect 1 1 2 2))

(deftest encoding
  (is (= #{:low-x :low-y} (sut/encode-endpoint (gv/vec2 0 0) r)))
  (is (= #{:low-x} (sut/encode-endpoint (gv/vec2 0 2) r)))
  (is (= #{:low-y} (sut/encode-endpoint (gv/vec2 2 0) r)))
  (is (= #{:high-x :high-y} (sut/encode-endpoint (gv/vec2 4 4) r)))
  (is (= #{:high-x} (sut/encode-endpoint (gv/vec2 4 2) r)))
  (is (= #{:high-y} (sut/encode-endpoint (gv/vec2 2 4) r))))

(deftest clip-line
  (is (nil? (sut/clip-line r (gv/vec2 4 4) (gv/vec2 5 4))))
  (is (nil? (sut/clip-line r (gv/vec2 4 4) (gv/vec2 4 2))))
  (is (nil? (sut/clip-line r (gv/vec2 0 0) (gv/vec2 10 0))))
  (is (line-delta= (gl/line2 1 1.5 2 2) (sut/clip-line r (gv/vec2 0 1) (gv/vec2 2 2))))
  (is (line-delta= (gl/line2 2 1 3 1.5) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 10 5))))
  (is (line-delta= (gl/line2 1 1 2 2) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 2 2))))
  (is (line-delta= (gl/line2 1 1 3 3) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 4 4))))
  (is (line-delta= (gl/line2 1 2 3 2) (sut/clip-line r (gv/vec2 0 2) (gv/vec2 4 2)))
      "horizontal line clipping")
  (is (line-delta= (gl/line2 2 1 2 3) (sut/clip-line r (gv/vec2 2 0) (gv/vec2 2 4)))
      "vertical line clipping"))

(deftest hatching
  (is (= 4 (count (sut/hatch-rectangle (rect/rect 2 2 4) 1.0 0.0)))))

(comment (t/run-tests))
