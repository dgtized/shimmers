(ns shimmers.math.geometry.ellipse-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest is]]
      :cljs [cljs.test :as t :include-macros true
             :refer [deftest is]])
   [shimmers.math.geometry.ellipse :as sut]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as m :refer [TWO_PI]]))

(deftest basics
  (is (m/delta= TWO_PI (g/area (sut/ellipse 2 1)))))
