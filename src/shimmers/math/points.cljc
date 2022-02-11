(ns shimmers.math.points
  (:require
   [shimmers.common.sequence :as cs]
   [thi.ng.geom.core :as g]
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

(defn ranked-pairs [points]
  (->> (for [[u v] (cs/all-pairs points)]
         [(g/dist u v) [u v]])
       (sort-by first)
       (map second)))
