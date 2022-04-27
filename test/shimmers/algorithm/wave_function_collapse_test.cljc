(ns shimmers.algorithm.wave-function-collapse-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest is]]
      :cljs [cljs.test :as t :refer-macros [deftest is] :include-macros true])
   [shimmers.algorithm.wave-function-collapse :as sut]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(deftest legal-rules
  (is (= [[:a (gv/vec2 0 1) :b]
          [:b (gv/vec2 1 0) :c]]
         (sut/legal-rules {:dims [2 2]
                           (gv/vec2 0 0) #{:a :b :c} (gv/vec2 1 0) #{:a :c}
                           (gv/vec2 0 1) #{:a :b} (gv/vec2 1 1) #{:a :b :c}}
                          [[:a (gv/vec2 1 0) :b]
                           [:a (gv/vec2 0 1) :b]
                           [:b (gv/vec2 1 0) :c]
                           [:b (gv/vec2 0 1) :c]]
                          (gv/vec2))))
  (is (= [[:a (gv/vec2 1 0) :a]
          [:b (gv/vec2 0 1) :b]]
         (sut/legal-rules {:dims [2 2]
                           (gv/vec2 0 0) #{:a :b} (gv/vec2 1 0) #{:a}
                           (gv/vec2 0 1) #{:b} (gv/vec2 1 1) #{:a :b}}
                          [[:a (gv/vec2 1 0) :a]
                           [:b (gv/vec2 1 0) :b]
                           [:a (gv/vec2 0 1) :a]
                           [:b (gv/vec2 0 1) :b]]
                          (gv/vec2)))))

(deftest entropy
  (is (tm/delta= 0.950270 (sut/entropy {(gv/vec2) #{:A :B :C}} {:A 3 :B 1 :C 1} (gv/vec2))))
  (is (tm/delta= 0.562335 (sut/entropy {(gv/vec2) #{:A :B}} {:A 3 :B 1 :C 1} (gv/vec2))))
  (is (tm/delta= 0.693147 (sut/entropy {(gv/vec2) #{:C :B}} {:A 3 :B 1 :C 1} (gv/vec2))))
  (is (tm/delta= 0 (sut/entropy {(gv/vec2) #{:A}} {:A 3 :B 1 :C 1} (gv/vec2))))
  (is (tm/delta= 0 (sut/entropy {(gv/vec2) #{:B}} {:A 3 :B 1 :C 1} (gv/vec2)))))

(comment (t/run-tests))

