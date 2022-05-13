(ns shimmers.automata.simplify-test
  (:require
   [cljs.test :as t :include-macros true
    :refer-macros [deftest is run-tests]]
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
           [:color [0 0 0 0]]])))
  (is (= [[:fork 2]
          [:one-of [[:fork 0]
                    [:forward 1]]]]
         (sut/simplify-program
          [[:fork 0]
           [:fork 0]
           [:one-of [[:fork 0]
                     [:forward 1]]]])))
  (is (= [[:rotate 1]
          [:forward 1]
          [:halt 0]]
         (sut/simplify-program
          [[:rotate 1]
           [:forward 1]
           [:halt 0]
           [:fork 0]]))
      "removes instructions after halt")
  (is (= [[:rotate 1]
          [:forward 1]
          [:one-of [[:goto 2]
                    [:halt 0]]]
          [:halt 0]
          [:fork 1]]
         (sut/simplify-program
          [[:rotate 1]
           [:forward 1]
           [:one-of [[:goto 2]
                     [:halt 0]]]
           [:halt 0]
           [:fork 1]]))
      "does not remove instructions after halt if a goto occurs prior")
  (is (= [[:rotate 1]
          [:one-of [[:goto 2]
                    [:halt 0]]]
          [:fork 1]]
         (sut/simplify-program
          [[:rotate 1]
           [:one-of [[:goto 2]
                     [:halt 0]]]
           [:fork 1]]))
      "does not remove instructions after a one-of halt"))

(deftest accept-program
  (is (not (sut/accept-program? [[:halt 0]])))
  (is (sut/accept-program? [[:rotate 0.1]
                            [:one-of [[:forward 1]
                                      [:halt 0]]]
                            [:halt 0]])))

(comment (run-tests))
