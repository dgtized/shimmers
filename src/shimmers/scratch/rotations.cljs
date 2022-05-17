(ns shimmers.scratch.rotations
  (:require
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.quaternion :as quat]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.equations :as eq]))

(comment
  (for [t (tm/norm-range 10)
        :let [theta (* eq/TAU t)
              a (gv/vec2)
              b (v/polar 1.0 theta)
              c (g/rotate (tm/- b a) (/ eq/TAU 6))
              axis (gv/vec3 (:xy (tm/- b a)))]]
    {:axis axis
     :quat (quat/quat-from-axis-angle axis tm/HALF_PI)
     :rot (g/rotate-around-axis (gv/vec3 (:xy c)) axis tm/HALF_PI)}))
