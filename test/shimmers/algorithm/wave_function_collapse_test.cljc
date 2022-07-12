(ns shimmers.algorithm.wave-function-collapse-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest is]]
      :cljs [cljs.test :as t :refer-macros [deftest is] :include-macros true])
   [shimmers.algorithm.wave-function-collapse :as sut]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(deftest rules-from-example
  (is (= [["A" (gv/vec2 1 0) "A"]
          ["A" (gv/vec2 0 1) "A"]
          ["A" (gv/vec2 -1 0) "A"]
          ["A" (gv/vec2 0 1) "B"]
          ["A" (gv/vec2 1 0) "B"]
          ["A" (gv/vec2 0 -1) "A"]
          ["B" (gv/vec2 -1 0) "A"]
          ["B" (gv/vec2 0 -1) "A"]]
         (sut/rules (sut/matrix->grid (sut/str->matrix "AA\nAB")
                                      sut/cardinal-directions))))
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
         (sut/rules (sut/matrix->grid (sut/str->matrix "AAA\nABA\nAAA")
                                      sut/cardinal-directions)))))

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

;; This is correct but seems off, as 1,1 is only place that can be B in test
;; case, but it's 4/5 likely to be assigned A
(deftest tile-weights
  (is (= {"A" 16 "B" 4}
         (sut/tile-weights
          (let [dirs sut/cardinal-directions
                grid (sut/matrix->grid (sut/str->matrix "AAA\nABA\nAAA")
                                       dirs)]
            (sut/legal-rules (assoc (sut/init-grid [3 3] dirs #{"A" "B"})
                                    (gv/vec2 1 0) #{"A"}
                                    (gv/vec2 0 1) #{"A"}
                                    (gv/vec2 2 1) #{"A"}
                                    (gv/vec2 1 2) #{"A"})
                             (sut/rules grid)
                             (gv/vec2 1 1)))))))

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
                        [:a (gv/vec2 0 -1) :b]]]
    (is (= [#{(gv/vec2 1 0) (gv/vec2 1 1) (gv/vec2 0 1)}
            {:dims [2 2]
             :directions sut/cardinal-directions
             (gv/vec2 0 0) #{:a} (gv/vec2 1 0) #{:b}
             (gv/vec2 0 1) #{:b} (gv/vec2 1 1) #{:a}}]
           (sut/propagate (sut/init-grid [2 2] sut/cardinal-directions #{:a :b})
                          alternating-ab (gv/vec2) #{:a})))
    (is (= [#{(gv/vec2 2 2) (gv/vec2 1 0) (gv/vec2 1 1) (gv/vec2 0 2)
              (gv/vec2 2 0) (gv/vec2 2 1) (gv/vec2 1 2) (gv/vec2 0 1)}
            {:dims [3 3]
             :directions sut/cardinal-directions
             (gv/vec2 0 0) #{:a} (gv/vec2 1 0) #{:b} (gv/vec2 2 0) #{:a}
             (gv/vec2 0 1) #{:b} (gv/vec2 1 1) #{:a} (gv/vec2 2 1) #{:b}
             (gv/vec2 0 2) #{:a} (gv/vec2 1 2) #{:b} (gv/vec2 2 2) #{:a}}]
           (sut/propagate (sut/init-grid [3 3] sut/cardinal-directions #{:a :b})
                          alternating-ab (gv/vec2) #{:a})))))

(deftest rules->tiles
  (is (= {[2 2] ["BA" "AA"]
          [0 0] ["AA" "AB"]
          [1 0] ["AB" "BC"]
          [1 1] ["BC" "AB"]
          [3 0] ["AA" "BA"]
          [0 2] ["AA" "AA"]
          [2 0] ["BA" "CB"]
          [3 1] ["BA" "AA"]
          [2 1] ["CB" "BA"]
          [1 2] ["AB" "AA"]
          [3 2] ["AA" "AA"]
          [0 1] ["AB" "AA"]}
         (sut/rules->tiles (sut/str->matrix "AABAA\nABCBA\nAABAA\nAAAAA") 2)))
  (is (= {[0 0] ["AAB" "ABC" "AAB"]
          [0 1] ["ABC" "AAB" "AAA"]
          [1 0] ["ABA" "BCB" "ABA"]
          [1 1] ["BCB" "ABA" "AAA"]
          [2 0] ["BAA" "CBA" "BAA"]
          [2 1] ["CBA" "BAA" "AAA"]}
         (sut/rules->tiles (sut/str->matrix "AABAA\nABCBA\nAABAA\nAAAAA") 3)))
  (is (= {[0 0] ["AABA" "ABCB" "AABA" "AAAA"]
          [1 0] ["ABAA" "BCBA" "ABAA" "AAAA"]}
         (sut/rules->tiles (sut/str->matrix "AABAA\nABCBA\nAABAA\nAAAAA") 4))))

(deftest rotations
  (is (= [["A"]] (sut/rotations ["A"])))
  (is (= [["BA" "AA"] ["AB" "AA"] ["AA" "AB"] ["AA" "BA"]]
         (sut/rotations ["BA" "AA"])))
  (is (= [["BAA" "AAA" "AAA"]
          ["AAB" "AAA" "AAA"]
          ["AAA" "AAA" "AAB"]
          ["AAA" "AAA" "BAA"]]
         (sut/rotations ["BAA" "AAA" "AAA"]))))

(deftest rules->rotated-tiles
  (is (= [["AAA" "ABB" "ABB"]
          ["AAA" "BBA" "BBA"]
          ["ABB" "ABB" "AAA"]
          ["BBA" "BBA" "AAA"]]
         (-> "AAAA\nABBA\nABBA\nAAAA"
             sut/str->matrix
             (sut/rules->rotated-tiles 3)))))

(deftest clockwise-faces
  (let [tile ["AAB" "AAA" "BBA"]]
    (is (= "AAB" (sut/clockwise-face tile 0)))
    (is (= "BAA" (sut/clockwise-face tile 1)))
    (is (= "ABB" (sut/clockwise-face tile 2)))
    (is (= "BAA" (sut/clockwise-face tile 3)))))

(deftest cardinal-face
  (let [tile ["AAB" "AAA" "BBA"]]
    (is (= "AAB" (sut/cardinal-face tile 0)))
    (is (= "BAA" (sut/cardinal-face tile 1)))
    (is (= "BBA" (sut/cardinal-face tile 2)))
    (is (= "AAB" (sut/cardinal-face tile 3)))))

(comment (t/run-tests))

