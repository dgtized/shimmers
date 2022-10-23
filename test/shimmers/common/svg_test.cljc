(ns shimmers.common.svg-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [clojure.walk :as walk]
   [shimmers.common.svg :as sut]
   [thi.ng.geom.rect :as rect]))

(defn remove-keys [tree]
  (walk/postwalk
   (fn [m]
     (if (map? m)
       (dissoc m :key)
       m))
   tree))

(deftest create-svg
  (is (= [:svg {:xmlns "http://www.w3.org/2000/svg"}]
         (remove-keys (sut/svg))))
  (is (= [:svg {:fill "red" :xmlns "http://www.w3.org/2000/svg"}]
         (remove-keys (sut/svg {:fill "red"}))))
  (is (= [:svg {:xmlns "http://www.w3.org/2000/svg"}
          [:rect {:x "0.00", :y "0.00", :width "10.00", :height "10.00"}]]
         (remove-keys (sut/svg {} (rect/rect 10)))))
  (is (= [:svg {:xmlns "http://www.w3.org/2000/svg"}
          [:rect {:x "0.00", :y "0.00", :width "10.00", :height "10.00"}]]
         (remove-keys (sut/svg {} [(rect/rect 10)]))))
  (is (= [:svg {:xmlns "http://www.w3.org/2000/svg"}
          [:rect {:x "0.00", :y "0.00", :width "5.00", :height "5.00"}]
          [:rect {:x "0.00", :y "0.00", :width "10.00", :height "10.00"}]]
         (remove-keys (sut/svg {} (rect/rect 5) (rect/rect 10)))))
  (is (= [:svg {:xmlns "http://www.w3.org/2000/svg"}
          [:rect {:x "0.00", :y "0.00", :width "5.00", :height "5.00"}]
          [:rect {:x "0.00", :y "0.00", :width "10.00", :height "10.00"}]]
         (remove-keys (sut/svg {} [(rect/rect 5) (rect/rect 10)]))))
  (is (= [:svg {:xmlns "http://www.w3.org/2000/svg"}
          [:rect {:x "0.00", :y "0.00", :width "5.00", :height "5.00"}]
          [:rect {:x "0.00", :y "0.00", :width "10.00", :height "10.00"}]
          [:rect {:x "0.00", :y "0.00", :width "15.00", :height "15.00"}]]
         (remove-keys (sut/svg {} [(rect/rect 5) (rect/rect 10)] [(rect/rect 15)])))))

(comment (t/run-tests))
