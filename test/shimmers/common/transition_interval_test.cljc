(ns shimmers.common.transition-interval-test
  (:require [clojure.test :as t :refer [deftest is] :include-macros true]
            [shimmers.common.transition-interval :as sut]
            [thi.ng.math.core :as tm]))

(deftest transitions
  (is (not (sut/complete? (sut/after 0 1) 0)))
  (is (not (sut/complete? (sut/after 0 1) 0.5)))
  (is (sut/complete? (sut/after 0 1) 1))

  (is (tm/delta= 0.0 (sut/percent (sut/after 0 10) 0)))
  (is (tm/delta= 0.1 (sut/percent (sut/after 0 10) 1)))
  (is (tm/delta= 1.0 (sut/percent (sut/after 0 10) 10)))
  (is (tm/delta= 1.1 (sut/percent (sut/after 0 10) 11))))
