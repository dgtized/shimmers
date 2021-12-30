(ns shimmers.math.geometry.triangle
  (:require
   [shimmers.math.geometry :as geometry]
   [shimmers.math.probability :as p]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.math.core :as tm]))

;; Longest edge is aesthetically more pleasing per:
;; https://tylerxhobbs.com/essays/2017/aesthetically-pleasing-triangle-subdivision
(defn longest-edge
  "Returns points of a triangle ordered from longest to shortest edge"
  [{[a b c] :points}]
  (let [dist-ab (g/dist a b)
        dist-bc (g/dist b c)
        dist-ca (g/dist c a)]
    (cond (and (>= dist-ab dist-bc) (>= dist-ab dist-ca))
          [a b c]
          (and (>= dist-ca dist-bc) (>= dist-ca dist-ab))
          [a c b]
          :else [b c a])))

(comment ;; TODO generalize for polygon points?
  (->> (gt/triangle2 [0 10] [0 3] [1 0])
       g/edges
       (sort-by (partial apply g/dist) #(compare %2 %1)))
  ;; Ranks edges, but need to extract unique points
  )

(defn decompose
  "Decompose triangle into a collection of smaller triangles"
  [t {:keys [mode inner-point sample sample-low sample-high]
      :or {mode :midpoint
           inner-point geometry/random-point-in-triangle
           sample (p/gaussian-clamped 0.5 0.1)
           sample-low (p/gaussian-clamped 0.33 0.1)
           sample-high (p/gaussian-clamped 0.33 0.1)}}]
  (let [[a b c] (longest-edge t)]
    (->> (case mode
           :midpoint
           (let [mid (tm/mix a b (sample))]
             [[a mid c]
              [b mid c]])
           :centroid
           (let [inner (inner-point t)]
             [[a b inner]
              [b c inner]
              [c a inner]])
           :inset
           (let [mab (tm/mix a b (sample))
                 mbc (tm/mix b c (sample))
                 mca (tm/mix c a (sample))]
             [[a mab mca]
              [b mab mbc]
              [c mbc mca]
              [mab mbc mca]])
           :trisect
           (let [[m1 m2] (->> [(sample-low) (sample-high)]
                              sort
                              (map (partial tm/mix a b)))]
             [[a m1 c]
              [b m2 c]
              [m1 m2 c]]))
         (mapv gt/triangle2))))

(defn decompose-largest [triangles]
  (let [[biggest & remaining] (sort-by g/area > triangles)]
    (concat remaining (decompose biggest {:mode :midpoint}))))
