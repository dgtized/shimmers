(ns shimmers.math.reflect-test
  (:require [shimmers.math.reflect :as sut]
            [cljs.test :as t :include-macros true
             :refer-macros [deftest is run-tests]]
            [thi.ng.math.core :as tm]))

(deftest mod-mixing
  (is (tm/delta= 0.9 (sut/mod-mix 0.1 0.7 0.5)))
  (is (tm/delta= 0.86 (sut/mod-mix 0.1 0.7 0.6)))
  (is (tm/delta= 0.02 (sut/mod-mix 0.1 0.9 0.4)))
  (is (tm/delta= 0 (sut/mod-mix 0.1 0.9 0.5)))
  (is (tm/delta= 0.98 (sut/mod-mix 0.1 0.9 0.6))))

(comment (run-tests))
