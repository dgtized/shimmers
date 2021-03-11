(ns shimmers.math.reflect-test
  (:require [shimmers.math.reflect :as sut]
            [cljs.test :as t :include-macros true
             :refer-macros [deftest is run-tests testing]]
            [thi.ng.math.core :as tm]))

(deftest mod-mixing
  (testing "normal mixing"
    (is (tm/delta= 0.2 (sut/mod-mix 0.1 0.3 0.5)))
    (is (tm/delta= 0.1 (sut/mod-mix 0.1 0.3 0)))
    (is (tm/delta= 0.3 (sut/mod-mix 0.1 0.3 1.0))))

  (testing "normal mixing inverted"
    (is (tm/delta= 0.2 (sut/mod-mix 0.3 0.1 0.5)))
    (is (tm/delta= 0.3 (sut/mod-mix 0.3 0.1 0)))
    (is (tm/delta= 0.1 (sut/mod-mix 0.3 0.1 1.0))))

  (testing "modular mixing"
    (is (tm/delta= 0 (sut/mod-mix 0.1 0.9 0.5)))
    (is (tm/delta= 0.02 (sut/mod-mix 0.1 0.9 0.4)))
    (is (tm/delta= 0.98 (sut/mod-mix 0.1 0.9 0.6)))
    (is (tm/delta= 0.1 (sut/mod-mix 0.1 0.9 0.0)))
    (is (tm/delta= 0.9 (sut/mod-mix 0.1 0.9 1.0))))

  (testing "modular mixing inverted"
    (is (tm/delta= 0 (sut/mod-mix 0.9 0.1 0.5)))
    (is (tm/delta= 0.98 (sut/mod-mix 0.9 0.1 0.4)))
    (is (tm/delta= 0.02 (sut/mod-mix 0.9 0.1 0.6)))
    (is (tm/delta= 0.9 (sut/mod-mix 0.9 0.1 0.0)))
    (is (tm/delta= 0.1 (sut/mod-mix 0.9 0.1 1.0)))))

(comment (run-tests))
