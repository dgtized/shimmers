(ns shimmers.algorithm.wave-function-collapse-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest is]]
      :cljs [cljs.test :as t :refer-macros [deftest is] :include-macros true])
   [shimmers.algorithm.wave-function-collapse :as sut]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(deftest rules-from-example
  (let [example (sut/str->matrix
                 "AA
                  AB")]
    (is (= [["A" (gv/vec2 1 0) "A"]
            ["A" (gv/vec2 0 1) "A"]
            ["A" (gv/vec2 -1 0) "A"]
            ["A" (gv/vec2 0 1) "B"]
            ["A" (gv/vec2 1 0) "B"]
            ["A" (gv/vec2 0 -1) "A"]
            ["B" (gv/vec2 -1 0) "A"]
            ["B" (gv/vec2 0 -1) "A"]]
           (sut/rules (sut/matrix->grid example)
                      sut/cardinal-directions))))
  (let [example (sut/str->matrix
                 "AAA
                  ABA
                  AAA")]
    (is (= [["A" (gv/vec2 1 0) "A"]
            ["A" (gv/vec2 0 1) "A"]
            ["A" (gv/vec2 1 0) "A"]
            ["A" (gv/vec2 -1 0) "A"]
            ["A" (gv/vec2 0 1) "B"]
            ["A" (gv/vec2 -1 0) "A"]
            ["A" (gv/vec2 0 1) "A"]
            ["A" (gv/vec2 1 0) "B"]
            ["A" (gv/vec2 0 1) "A"]
            ["A" (gv/vec2 0 -1) "A"]
            ["B" (gv/vec2 1 0) "A"]
            ["B" (gv/vec2 -1 0) "A"]
            ["B" (gv/vec2 0 1) "A"]
            ["B" (gv/vec2 0 -1) "A"]
            ["A" (gv/vec2 -1 0) "B"]
            ["A" (gv/vec2 0 1) "A"]
            ["A" (gv/vec2 0 -1) "A"]
            ["A" (gv/vec2 1 0) "A"]
            ["A" (gv/vec2 0 -1) "A"]
            ["A" (gv/vec2 1 0) "A"]
            ["A" (gv/vec2 -1 0) "A"]
            ["A" (gv/vec2 0 -1) "B"]
            ["A" (gv/vec2 -1 0) "A"]
            ["A" (gv/vec2 0 -1) "A"]]
           (sut/rules (sut/matrix->grid example) sut/cardinal-directions)))))

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
          [:a (gv/vec2 0 1) :a]
          [:b (gv/vec2 0 1) :b]]
         (sut/legal-rules {:dims [2 2]
                           (gv/vec2 0 0) #{:a :b} (gv/vec2 1 0) #{:a}
                           (gv/vec2 0 1) #{:a :b} (gv/vec2 1 1) #{:a :b}}
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

(deftest tiles-from-rules
  (is (= #{:a} (sut/tiles-from-rules [[:a (gv/vec2 1 0) :a]]
                                     [(gv/vec2 1 0)]))
      "allows tile if only direction support")
  (is (= #{} (sut/tiles-from-rules [[:a (gv/vec2 1 0) :a]]
                                   [(gv/vec2 1 0) (gv/vec2 0 1)]))
      "tile is not allowed if missing expected direction")
  (is (= #{:a} (sut/tiles-from-rules [[:a (gv/vec2 1 0) :a]
                                      [:a (gv/vec2 0 1) :a]
                                      [:a (gv/vec2 -1 0) :a]
                                      [:a (gv/vec2 0 -1) :a]]
                                     sut/cardinal-directions))
      "allows tile if all directions support")
  (is (= #{:a :b} (sut/tiles-from-rules [[:a (gv/vec2 1 0) :a]
                                         [:b (gv/vec2 1 0) :b]
                                         [:c (gv/vec2 1 0) :c]
                                         [:a (gv/vec2 0 1) :b]
                                         [:b (gv/vec2 0 1) :b]]
                                        [(gv/vec2 1 0) (gv/vec2 0 1)]))
      "allows tiles if all directions support that option")
  (is (empty? (sut/tiles-from-rules [[:a (gv/vec2 1 0) :a]
                                     [:b (gv/vec2 1 0) :b]
                                     [:c (gv/vec2 0 1) :b]]
                                    [(gv/vec2 1 0) (gv/vec2 0 1)]))
      "empty if a direction does not have overlapping legal tiles")
  (is (empty? (sut/tiles-from-rules [[:a (gv/vec2 1 0) :a]
                                     [:b (gv/vec2 1 0) :b]
                                     [:a (gv/vec2 0 1) :b]]
                                    sut/cardinal-directions))
      "empty if a direction does not have overlapping legal tiles"))

(deftest propagate
  (let [alternating-ab [[:b (gv/vec2 1 0) :a]
                        [:b (gv/vec2 0 1) :a]
                        [:b (gv/vec2 -1 0) :a]
                        [:b (gv/vec2 0 -1) :a]
                        [:a (gv/vec2 1 0) :b]
                        [:a (gv/vec2 0 1) :b]
                        [:a (gv/vec2 -1 0) :b]
                        [:a (gv/vec2 0 -1) :b]
                        ]]
    (is (= [#{(gv/vec2 1 0) (gv/vec2 1 1) (gv/vec2 0 1)}
            {:dims [2 2]
             (gv/vec2 0 0) #{:a} (gv/vec2 1 0) #{:b}
             (gv/vec2 0 1) #{:b} (gv/vec2 1 1) #{:a}}]
           (sut/propagate (sut/init-grid [2 2] #{:a :b})
                          alternating-ab (gv/vec2) #{:a})))
    (is (= [#{(gv/vec2 2 2) (gv/vec2 1 0) (gv/vec2 1 1) (gv/vec2 0 2)
              (gv/vec2 2 0) (gv/vec2 2 1) (gv/vec2 1 2) (gv/vec2 0 1)}
            {:dims [3 3]
             (gv/vec2 0 0) #{:a} (gv/vec2 1 0) #{:b} (gv/vec2 2 0) #{:a}
             (gv/vec2 0 1) #{:b} (gv/vec2 1 1) #{:a} (gv/vec2 2 1) #{:b}
             (gv/vec2 0 2) #{:a} (gv/vec2 1 2) #{:b} (gv/vec2 2 2) #{:a}
             }]
           (sut/propagate (sut/init-grid [3 3] #{:a :b})
                          alternating-ab (gv/vec2) #{:a})))))

(comment (t/run-tests))

