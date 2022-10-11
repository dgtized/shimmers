(ns shimmers.math.geometry.line
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [Line2 Line3]]))
  #?(:clj
     (:import [thi.ng.geom.types Line2 Line3])))

(extend-type Line2
  g/IHeading
  (heading [{[p q] :points}]
    (g/heading (tm/- q p)))
  (heading-xy [{[p q] :points}]
    (g/heading-xy (tm/- q p)))
  (angle-between [{[p q] :points} v]
    (g/angle-between (tm/- q p) (gv/vec2 v)))
  (slope-xy [{[p q] :points}]
    (g/slope-xy (tm/- q p))))

(extend-type Line3
  g/IHeading
  (heading [{[p q] :points}]
    (g/heading (tm/- q p)))
  (heading-xy [{[p q] :points}]
    (g/heading-xy (tm/- q p)))
  (heading-xz [{[p q] :points}]
    (g/heading-xz (tm/- q p)))
  (heading-yz [{[p q] :points}]
    (g/heading-yz (tm/- q p)))
  (angle-between [{[p q] :points} v]
    (g/angle-between (tm/- q p) (gv/vec3 v)))
  (slope-xy [{[p q] :points}]
    (g/slope-xy (tm/- q p)))
  (slope-xz [{[p q] :points}]
    (g/slope-xz (tm/- q p)))
  (slope-yz [{[p q] :points}]
    (g/slope-yz (tm/- q p))))

