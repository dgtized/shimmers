(ns shimmers.common.sequence-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.common.sequence :as sut]))

(t/deftest rotate
  (t/is (= [1 2 3] (sut/rotate 0 [1 2 3])))
  (t/is (= [2 3 1] (sut/rotate 1 [1 2 3])))
  (t/is (= [3 1 2] (sut/rotate -1 [1 2 3])))
  (t/is (= [3 4 1 2] (sut/rotate 2 [1 2 3 4])))
  (t/is (= [2 3 4 1] (sut/rotate -3 [1 2 3 4]))))

(t/deftest weighted
  (t/is (= [] (sut/weighted)))
  (t/is (= [:a :a] (sut/weighted 2 :a)))
  (t/is (= [:a :b :b] (sut/weighted 1 :a 2 :b))))

(t/deftest separate
  (t/is (= [[] []] (sut/separate #(> % 1) [])))
  (t/is (= [[3 2] [1 0]] (sut/separate #(> % 1) (range 4))))
  (t/is (= [[2 3] [0 1]] (sut/separate #(> % 1) (vec (range 4))))))

(deftest middle
  (is (= 4 (sut/middle (range 8))))
  (is (= 4 (sut/middle (range 9))))
  (is (= 5 (sut/middle (range 10))))
  (is (= 5 (sut/middle (range 11)))))

(deftest map-with-window
  (is (= [[1 [0 1]] [2 [0 1]]
          [3 [1 2]] [4 [2 3]]
          [5 [3 4]] [6 [4 5]]]
         (sut/map-with-window 2 (fn [x w] [(inc x) w]) (range 6))))
  (is (= [[0 [0 1 2]]
          [1 [0 1 2]]
          [2 [1 2 3]]
          [3 [2 3 4]]
          [4 [3 4 5]]
          [5 [4 5 6]]
          [6 [5 6 7]]
          [7 [5 6 7]]]
         (sut/map-with-window 3 (fn [x w] [x w]) (range 8))))
  (is (= [[0 [0 1 2 3]]
          [1 [0 1 2 3]]
          [2 [0 1 2 3]]
          [3 [1 2 3 4]]
          [4 [2 3 4 5]]
          [5 [3 4 5 6]]
          [6 [4 5 6 7]]
          [7 [4 5 6 7]]]
         (sut/map-with-window 4 (fn [x w] [x w]) (range 8))))
  (is (= [[1 [0 1 2 3 4]]
          [2 [0 1 2 3 4]]
          [3 [0 1 2 3 4]]
          [4 [1 2 3 4 5]]
          [5 [2 3 4 5 6]]
          [6 [3 4 5 6 7]]
          [7 [3 4 5 6 7]]
          [8 [3 4 5 6 7]]]
         (sut/map-with-window 5 (fn [x w] [(inc x) w]) (range 8))))
  (is (= [[1 [0 1 2 3]]
          [2 [0 1 2 3]]
          [3 [0 1 2 3]]
          [4 [0 1 2 3]]]
         (sut/map-with-window 4 (fn [x w] [(inc x) w]) (range 4))))
  (is (= [[1 [0 1 2]]
          [2 [0 1 2]]
          [3 [0 1 2]]]
         (sut/map-with-window 4 (fn [x w] [(inc x) w]) (range 3)))
      "Allows window sizes > than the number of elements in coll"))

(deftest partition-segments
  (is (= [[0 0] [2 4] [7 7] [9 11] [14 14]]
         (sut/partition-segments (cycle [1 3]) (cycle [1 2]) (range 15))))
  (is (= [[0 0] [1 2] [4 6] [7 7] [9 9]]
         (sut/partition-segments (cycle [1 2 3]) (cycle [0 1]) (range 10))))
  (is (= [[nil nil] [0 0] [nil nil] [2 2]]
         (sut/partition-segments (cycle [0 1]) (cycle [0 1]) (range 4)))))

(deftest partition-chunks
  (is (= [[0] [1 2] [3]]
         (sut/partition-chunks [1 2 3] (range 4))))
  (is (= [[0] [1 2] [3 4 5] [6 7]]
         (sut/partition-chunks [1 2 3] (range 8))))
  (is (= [[0] [1 2] [3 4 5] [6] [7 8] [9]]
         (sut/partition-chunks (cycle [1 2 3]) (range 10)))))

(deftest collapsable
  (t/are [in out] (= out (sut/collapse = + in))
    [] []
    [1] [1]
    [1 1] [2]
    [1 1 2] [4] ;; 1+1 becomes 2, which is same as 2
    [1 2 1 1] [1 2 2])
  (is (= [{:v 1, :x 3} {:v 2, :x 2}]
         (sut/collapse (fn [a b] (= (:v a) (:v b)))
                       (fn [a b] (update a :x + (:x b)))
                       [{:v 1 :x 1} {:v 1 :x 2} {:v 2 :x 2}]))))

(deftest iterate-cyles
  (is (= 0 (sut/iterate-cycles 0 inc 0)))
  (is (= 5 (sut/iterate-cycles 5 inc 0))))

(deftest iterate-fixed
  (is (= [] (sut/iterate-fixed-point
             (fn [x] (drop 1 x)) (range 5)))))

(deftest non-consecutive-pairs
  (is (= [[0 2] [0 3] [0 4] [1 3] [1 4] [2 4]]
         (sut/non-consecutive-pairs (range 5)))))

(deftest take-until
  (is (= [0] (sut/take-until #(= 0 %) (range 10))))
  (is (= [0 1] (sut/take-until #(= 1 %) (range 10)))))

(deftest drop-until
  (is (= [] (sut/drop-until #(> % 4) (range 4))))
  (is (= [2 3] (sut/drop-until #(= % 2) (range 4))))
  (is (= [2 3] (sut/drop-until #(>= % 2) (range 4)))))

(comment (t/run-tests))
