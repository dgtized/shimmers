(ns shimmers.algorithm.wave-function-collapse-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest is]]
      :cljs [cljs.test :as t :refer-macros [deftest is] :include-macros true])
   [shimmers.algorithm.wave-function-collapse :as sut]
   [thi.ng.geom.vector :as gv]))

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

(comment (t/run-tests))

