(ns shimmers.algorithm.rtree-test
  (:require [cljs.test :as t :refer-macros [deftest is] :include-macros true]
            [clojure.set :as set]
            [shimmers.algorithm.rtree :as sut]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.utils :as gu]))

(deftest creation
  (is (nil? (sut/create [])))
  (let [circles (repeatedly 10 #(gc/circle (rand-int 100) (rand-int 100) (rand-int 10)))
        bounds (gu/coll-bounds circles)
        tree (sut/create circles)
        search (sut/search-intersection tree bounds)
        example (first circles)]
    (is (= (set circles) (set search)))
    (is (= (set circles) (set (sut/search-intersection tree (rect/rect 0 0 100 100)))))
    (is (set/subset? (set [example]) (set (sut/search-intersection tree (g/bounds example)))))))

(comment (t/run-tests))

(comment (sut/create (repeatedly 30 #(gc/circle (rand-int 100) (rand-int 100) 1))))
