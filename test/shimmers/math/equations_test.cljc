(ns shimmers.math.equations-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.equations :as sut]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(deftest cos-similarity
  (is (tm/delta= 1.0 (sut/cos-similarity (gv/vec2 1 0) (gv/vec2 2 0))))
  (is (tm/delta= -1.0 (sut/cos-similarity (gv/vec2 1 0) (gv/vec2 -1 0))))
  (is (tm/delta= 0.0 (sut/cos-similarity (gv/vec2 1 0) (gv/vec2 0 1)))))
