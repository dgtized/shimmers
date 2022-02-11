(ns shimmers.math.points
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as gv]))

(defn generate
  "Generate point 2d points in space"
  ([n dist]
   (generate n dist dist))
  ([n dist-x dist-y] (repeatedly n #(gv/vec2 (dist-x) (dist-y)))))

(defn minimum-separation
  "Only keep `points` if closest pair distance is greather than `threshold`."
  [threshold points]
  (reduce (fn [accepted p]
            (if (every? #(> (g/dist p %) threshold) accepted)
              (conj accepted p)
              accepted))
          [] points))
