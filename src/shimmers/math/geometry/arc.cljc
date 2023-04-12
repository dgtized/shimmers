(ns shimmers.math.geometry.arc
  (:require
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; t0 & t1 represent the start and end angles of an arc sector
(defrecord Arc [p r t0 t1]
  g/ICenter
  (centroid [_]
    (let [alpha (/ (- t1 t0) 2)]
      (gv/vec2 (/ (* 2 r (Math/sin alpha)) (* 3 alpha))
               (:y (v/+polar p r (+ t0 alpha))))))

  g/IVertexAccess
  (vertices [_] (g/vertices _ 12))
  (vertices [{:keys [p r t0 t1]} res]
    (->> res
         tm/norm-range
         (mapv (fn [t] (v/+polar p r (tm/mix* t0 t1 t))))
         (into [p])))

  g/ISample
  (random-point [_]
    (v/+polar p r (dr/random t0 t1)))
  (random-point-inside [_]
    (v/+polar p (* r (Math/sqrt (dr/random)))
              (dr/random t0 t1))))
