(ns shimmers.algorithm.rtree-test
  (:require [cljs.test :as t :refer-macros [deftest is] :include-macros true]
            [shimmers.algorithm.rtree :as sut]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]))

(deftest creation
  (is (nil? (sut/create [])))
  (let [circles (repeatedly 10 #(gc/circle (rand) (rand) (rand)))
        bounds (sut/compute-bounds (map geom/bounds circles))
        tree (sut/create circles)
        search (sut/search-intersection tree bounds)]
    (is (= (set circles) (set search)))))

(comment (t/run-tests))
