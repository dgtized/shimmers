(ns shimmers.math.geometry
  (:require [shimmers.math.probability :as p]
            [thi.ng.geom.core :as geom]
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

