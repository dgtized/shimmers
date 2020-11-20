(ns shimmers.automata.simplify-test
  (:require [cljs.test :as t :include-macros true
             :refer-macros [deftest is testing run-tests]]
            [shimmers.automata.simplify :as sut]))

(deftest collapse-trivial-one-of
  (is (empty? (mapcat sut/collapse-trivial-one-of [[:one-of []]])))
  (is (= [[:forward 1]]
         (mapcat sut/collapse-trivial-one-of
                 [[:one-of [[:forward 1]]]])))
  (is (= [[:one-of [[:forward 1] [:forward 2]]]]
         (mapcat sut/collapse-trivial-one-of
                 [[:one-of [[:forward 1]
                            [:one-of [[:forward 2]]]]]]))))

(deftest simplify-program
  (is (= [[:color [:gradient :rainbow1]]
          [:rotate 0.30000000000000004]
          [:rotate [:random 5]]
          [:rotate [:random 5]]
          [:color [0 0 0 0]]]
         (sut/simplify-program
          [[:color [:gradient :rainbow1]]
           [:color [:gradient :rainbow1]]
           [:rotate 0.1]
           [:rotate 0.2]
           [:rotate [:random 5]]
           [:rotate [:random 5]]
           [:color [0 0 0 0]]]))))

(deftest accept-program
  (is (not (sut/accept-program? [[:halt 0]])))
  (is (sut/accept-program? [[:one-of [[:forward 1]
                                      [:halt 0]]]
                            [:halt 0]])))

(run-tests)
