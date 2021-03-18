(ns shimmers.math.geometry
  (:require [shimmers.math.probability :as p]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.quaternion :as quat]
            thi.ng.geom.polygon
            [thi.ng.geom.triangle :as gt]
            #?(:clj [thi.ng.geom.types] :cljs [thi.ng.geom.types :refer [Polygon2]])
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm])
  #?(:clj (:import [thi.ng.geom.types Polygon2])))

;; http://extremelearning.com.au/evenly-distributing-points-in-a-triangle/
;; https://stackoverflow.com/questions/47410054/generate-random-locations-within-a-triangular-domain/47418580#47418580
(defn random-point-in-triangle2 [{:keys [points]}]
  (let [[s t] (sort [(rand) (rand)])
        weighting (gv/vec3 s (- t s) (- 1 t))]
    (gv/vec2 (tm/dot (apply gv/vec3 (map :x points)) weighting)
             (tm/dot (apply gv/vec3 (map :y points)) weighting))))

;; Uniformly sample points from tesselated triangles of polygon
;; https://blogs.sas.com/content/iml/2020/10/21/random-points-in-polygon.html
;; https://observablehq.com/@scarysize/finding-random-points-in-a-polygon
(extend-type Polygon2
  geom/ISample
  (random-point-inside
    [_] (->> (geom/tessellate _)
             (map gt/triangle2)
             (p/weighted-by geom/area)
             random-point-in-triangle2)))

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
  ;; Ranks edges, but need to extract unique points)

(defn decompose
  "Decompose triangle into a collection of smaller triangles"
  [t {:keys [mode inner-point sample]
      :or {mode :midpoint
           inner-point geom/random-point-inside
           sample (fn [] (+ 0.25 (* 0.5 (rand))))}}]
  (let [[a b c] (longest-edge t)]
    (case mode
      :midpoint
      (let [mid (tm/mix a b (sample))]
        [(gt/triangle2 a mid c)
         (gt/triangle2 b mid c)])
      :centroid
      (let [inner (inner-point t)]
        [(gt/triangle2 a b inner)
         (gt/triangle2 b c inner)
         (gt/triangle2 c a inner)])
      :inset
      (let [mab (tm/mix a b (sample))
            mbc (tm/mix b c (sample))
            mca (tm/mix c a (sample))]
        [(gt/triangle2 a mab mca)
         (gt/triangle2 b mab mbc)
         (gt/triangle2 c mbc mca)
         (gt/triangle2 mab mbc mca)]))))
