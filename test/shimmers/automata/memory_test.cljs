(ns shimmers.automata.memory-test
  (:require [cljs.test :as t :include-macros true
             :refer-macros [deftest is]]
            [shimmers.automata.memory :as sut]))

(def pages 8)
(def a1 (sut/->Allocation 0 2 2))
(def a2 (sut/->Allocation 0 4 1))

(deftest next-free-chunk
  (is (= 0 (sut/next-free pages [a1] 0)))
  (is (= 4 (sut/next-free pages [a1] 2)))
  (is (= 4 (sut/next-free pages [a1] 4)))
  (is (= 5 (sut/next-free pages [a1] 5)))
  (is (= 0 (sut/next-free pages [a1] 8))))

(deftest next-boundary
  (is (= 2 (sut/next-bounds 8 [a1] 0)))
  (is (= 4 (sut/next-bounds 8 [a1 a2] 3)))
  (is (= 8 (sut/next-bounds 8 [a1] 5))))

(comment (t/run-tests))
