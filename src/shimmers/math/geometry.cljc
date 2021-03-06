(ns shimmers.math.geometry
  (:require [shimmers.math.probability :as p]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.quaternion :as quat]
            thi.ng.geom.polygon
            [thi.ng.geom.triangle :as gt]
            #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Polygon2 Line2 Line3]])
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm])
  #?(:clj (:import [thi.ng.geom.types Polygon2 Line2 Line3])))

(defn generate-points
  "Generate point 2d points in space"
  ([n dist]
   (generate-points n dist dist))
  ([n dist-x dist-y] (repeatedly n #(gv/vec2 (dist-x) (dist-y)))))

;; Kraemer Method
;; http://extremelearning.com.au/evenly-distributing-points-in-a-triangle/
;; https://stackoverflow.com/questions/47410054/generate-random-locations-within-a-triangular-domain/47418580#47418580
(defn random-point-in-triangle [{:keys [points]}]
  (let [[s t] (sort [(rand) (rand)])
        weighting [s (- t s) (- 1 t)]]
    (apply tm/+ (map tm/* points weighting))))

;; https://stats.stackexchange.com/questions/481543/generating-random-points-uniformly-on-a-disk
(defn random-point-in-circle [_]
  (-> (gv/vec2
       (* (get _ :r) (Math/sqrt (tm/random)))
       (* tm/TWO_PI (tm/random)))
      geom/as-cartesian
      (tm/+ (get _ :p))))

;; Uniformly sample points from tesselated triangles of polygon
;; https://blogs.sas.com/content/iml/2020/10/21/random-points-in-polygon.html
;; https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
(extend-type Polygon2
  geom/ISample
  (random-point-inside
    [_] (->> (geom/tessellate _)
             (map gt/triangle2)
             (p/weighted-by geom/area)
             random-point-in-triangle)))

;; TODO: remove once https://github.com/thi-ng/geom/pull/82 is published
(extend-type Line2
  geom/IFlip
  (flip [_]
    (Line2. (vec (rseq (:points _))))))

(extend-type Line3
  geom/IFlip
  (flip [_]
    (Line3. (vec (rseq (:points _))))))

(defn rotate-around-centroid [polygon t]
  (-> polygon
      geom/center
      (geom/rotate t)
      (geom/translate (geom/centroid polygon))))

;; Quaternion
;; https://www.weizmann.ac.il/sci-tea/benari/sites/sci-tea.benari/files/uploads/softwareAndLearningMaterials/quaternion-tutorial-2-0-1.pdf
;; http://danceswithcode.net/engineeringnotes/quaternions/quaternions.html
;; https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
;; https://en.wikipedia.org/wiki/Gimbal_lock
;; https://developerblog.myo.com/quaternions/
(defn rotate-over-edge [poly [a b] theta]
  (let [axis (tm/- b a)
        rotation (quat/quat-from-axis-angle axis theta)]
    (-> poly
        (geom/translate (tm/- a))
        (geom/transform rotation)
        (geom/translate a))))

(defn reflect-over-edge [c [a b]]
  (let [edge (gl/line3 a b)
        close (gl/line3 c (geom/closest-point edge c))]
    (first (:points (geom/reflect close edge)))))

(defn displace [polygon theta dir]
  (-> polygon
      geom/center
      (geom/rotate theta)
      (geom/translate (tm/+ (geom/centroid polygon) dir))))

(defn radial-sort
  "Counter-clockwise sort of all points around an origin point"
  [origin points]
  (sort-by (fn [p] (geom/heading (tm/- p origin))) points))

(defn shape-at [shape rotation scale pos]
  (-> shape
      (geom/rotate rotation)
      (geom/scale-size scale)
      (geom/translate pos)))

;; Longest edge is aesthetically more pleasing per:
;; https://tylerxhobbs.com/essays/2017/aesthetically-pleasing-triangle-subdivision
(defn longest-edge
  "Returns points of a triangle ordered from longest to shortest edge"
  [{[a b c] :points}]
  (let [dist-ab (geom/dist a b)
        dist-bc (geom/dist b c)
        dist-ca (geom/dist c a)]
    (cond (and (>= dist-ab dist-bc) (>= dist-ab dist-ca))
          [a b c]
          (and (>= dist-ca dist-bc) (>= dist-ca dist-ab))
          [a c b]
          :else [b c a])))

(comment ;; TODO generalize for polygon points?
  (->> (gt/triangle2 [0 10] [0 3] [1 0])
       geom/edges
       (sort-by (partial apply geom/dist) #(compare %2 %1)))
  ;; Ranks edges, but need to extract unique points
  )

(defn decompose
  "Decompose triangle into a collection of smaller triangles"
  [t {:keys [mode inner-point sample sample-low sample-high]
      :or {mode :midpoint
           inner-point geom/random-point-inside
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

(defn confused-midpoint
  "For a given line p to q, pick a random point from the circle centered at the
  midpoint. d is a sizing factor for the radius, d of 1 yields a circle that clips
  p and q, d of 0.5 would only allow points in the middle half."
  [p q d]
  (->> (* d 0.5 (geom/dist p q))
       (p/confusion-disk (tm/mix p q 0.5))
       gv/vec2))

(defn circles-overlap? [a b]
  (let [distance (+ (:r a) (:r b))]
    (< (geom/dist (:p a) (:p b)) distance)))
