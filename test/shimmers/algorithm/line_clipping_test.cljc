(ns shimmers.algorithm.line-clipping-test
  (:require [shimmers.algorithm.line-clipping :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is testing]]
               :cljs [cljs.test :as t :include-macros true
                      :refer-macros [deftest is testing]])
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]))

(def r (rect/rect 1 1 2 2))

(deftest encoding
  (is (= #{:low-x :low-y} (sut/encode-endpoint (gv/vec2 0 0) r)))
  (is (= #{:low-x} (sut/encode-endpoint (gv/vec2 0 2) r)))
  (is (= #{:low-y} (sut/encode-endpoint (gv/vec2 2 0) r)))
  (is (= #{:high-x :high-y} (sut/encode-endpoint (gv/vec2 4 4) r)))
  (is (= #{:high-x} (sut/encode-endpoint (gv/vec2 4 2) r)))
  (is (= #{:high-y} (sut/encode-endpoint (gv/vec2 2 4) r))))

(deftest clip-line
  (is (= (gl/line2 1 1.5 2 2) (sut/clip-line r (gv/vec2 0 1) (gv/vec2 2 2))))
  (is (= (gl/line2 1 1 2 2) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 2 2))))
  (is (= (gl/line2 1 1 3 3) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 4 4))))
  (is (= (gl/line2 1 2 3 2) (sut/clip-line r (gv/vec2 0 2) (gv/vec2 4 2)))
      "horizontal line clipping")
  (is (= (gl/line2 2 1 2 3) (sut/clip-line r (gv/vec2 2 0) (gv/vec2 2 4)))
      "vertical line clipping"))

(comment (t/run-tests))
