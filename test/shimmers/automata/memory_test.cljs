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

(def A sut/->Allocation)
(def single-alloc [(A 1 4 2)])
(def split-alloc [(A 2 4 1) (A 2 6 1)])

(deftest allocation
  (is (= [(A 2 0 2)] (sut/allocate 2 8 single-alloc 2 0))
      "places before the existing allocation")
  (is (= [(A 2 2 2)] (sut/allocate 2 8 single-alloc 2 2))
      "places before existing alloc, but relative to start")
  (is (= [(A 2 6 1) (A 2 2 2)] (sut/allocate 2 8 single-alloc 3 2))
      "places upto existing alloc, and remaining after")
  (is (= [(A 2 0 2) (A 2 6 2)] (sut/allocate 2 8 single-alloc 4 4))
      "places after existing alloc, and remaining that is larger than page starts from 0")
  (is (= [(A 2 6 2)] (sut/allocate 2 8 single-alloc 2 6))
      "places after existing alloc, relative to start")
  (is (= [(A 2 0 2)] (sut/allocate 2 8 single-alloc 2 8))
      "places before existing alloc as start is at page boundary")
  (is (= [(A 3 5 1) (A 3 0 4)]
         (sut/allocate 3 8 split-alloc 5 0))
      "splits first 4 allocations prior to existing, and last between")
  (is (= [(A 3 7 1) (A 3 5 1) (A 3 1 3)]
         (sut/allocate 3 8 split-alloc 5 1))
      "splits into all the remaining gaps in memory"))

(comment (t/run-tests))
