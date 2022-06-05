(ns shimmers.algorithm.quadtree-test
  (:require [shimmers.algorithm.quadtree :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :refer-macros [deftest is] :include-macros true])
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.core :as g]))

(deftest deletion
  (let [c1 (gc/circle 0 0 1)
        c1' (gc/circle 0 0 2)
        c2 (gc/circle 0 1 1)
        c3 (gc/circle 1 1 1)
        c4 (gc/circle -1 2 1)
        q (-> (sut/circletree -10 -10 20 20)
              (g/add-point (:p c1) c1))]
    (is (= [c1] (spatialtree/select-with-shape q (g/bounds q))))
    (is (= [] (spatialtree/select-with-shape (g/delete-point q (:p c1)) (g/bounds q))))
    (is (= [c1'] (spatialtree/select-with-shape (sut/replace-point q (:p c1) c1') (g/bounds q))))

    (let [q2 (reduce (fn [g c] (g/add-point g (:p c) c))
                     (sut/circletree -10 -10 20 20)
                     [c1 c2 c3 c4])]
      (is (= (set [c1 c2 c3 c4]) (set (sut/all-data q2))))
      (is (= (set [c1' c2 c3 c4])
             (set (sut/all-data (sut/replace-point q2 (:p c1) c1'))))))))

(comment (t/run-tests))
