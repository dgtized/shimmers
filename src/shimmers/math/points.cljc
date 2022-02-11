(ns shimmers.math.points
  (:require [thi.ng.geom.core :as g]))

(defn minimum-separation
  "Only keep `points` if closest pair distance is greather than `threshold`."
  [threshold points]
  (reduce (fn [accepted p]
            (if (every? #(> (g/dist p %) threshold) accepted)
              (conj accepted p)
              accepted))
          [] points))
