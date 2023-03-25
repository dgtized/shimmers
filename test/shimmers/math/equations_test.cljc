(ns shimmers.math.equations-test
  (:require [shimmers.math.equations :as sut]
            [clojure.test :as t :refer [deftest is] :include-macros true]
            [thi.ng.geom.vector :as gv]))

(deftest cos-similarity
  (is (= 1 (sut/cos-similarity (gv/vec2 1 0) (gv/vec2 2 0))))
  (is (= -1 (sut/cos-similarity (gv/vec2 1 0) (gv/vec2 -1 0))))
  (is (= 0 (sut/cos-similarity (gv/vec2 1 0) (gv/vec2 0 1)))))
