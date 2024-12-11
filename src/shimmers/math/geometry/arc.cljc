(ns shimmers.math.geometry.arc
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defprotocol IHalfAngle
  (half-angle [_]))

;; t0 & t1 represent the start and end angles of an arc sector
;; t0 must be clockwise of t1 in modular space
(defrecord Arc [p r t0 t1]
  IHalfAngle
  (half-angle [_]
    (let [t0' (mod t0 eq/TAU)
          t1' (mod t1 eq/TAU)]
      (if (< t1' t0')
        (/ (- (+ t1' eq/TAU) t0') 2)
        (/ (- t1' t0') 2))))

  g/IArea
  (area [_] (* (half-angle _) r r))

  g/IBounds
  ;; FIXME: this is the circle bounds, tighten to arc bounds?
  (bounds [_]
    (rect/rect (tm/- p r) (gv/vec2 (* 2 r))))
  (width [_] (* 2 r))
  (height [_] (* 2 r))
  (depth [_] 0)

  g/IBoundary
  (contains-point? [_ q]
    (let [rel (tm/- q p)]
      (and (<= (tm/mag rel) r)
           (sm/radians-between? t0 t1 (g/heading rel)))))

  g/ICenter
  (centroid [_]
    (let [alpha (half-angle _)]
      (gv/vec2 (/ (* 2 r (math/sin alpha)) (* 3 alpha))
               (:y (v/+polar p r (+ t0 alpha))))))

  g/IVertexAccess
  (vertices [_] (g/vertices _ 12))
  (vertices [{:keys [p r t0 t1]} res]
    (->> res
         tm/norm-range
         (mapv (fn [t] (v/+polar p r (tm/mix* t0 t1 t))))
         (into [p])))

  g/ISample
  (point-at [_ t]
    (v/+polar p r (tm/mix* t0 t1 t)))
  (random-point [_]
    ;; note, does not include edges to center, just arc surface
    (v/+polar p r (dr/random t0 t1)))
  (random-point-inside [_]
    (v/+polar p (* r (math/sqrt (dr/random)))
              (dr/random t0 t1)))

  rp/ISamplePoint
  (sample-point-at [_ t]
    (v/+polar p r (tm/mix* t0 t1 t)))
  (sample-point-bounds [_]
    (v/+polar p r (dr/random t0 t1)))
  (sample-point-inside [_]
    (v/+polar p (* r (math/sqrt (dr/random)))
              (dr/random t0 t1)))

  svg/ISVGConvert
  (as-svg [_ opts]
    (csvg/path [[:M (v/+polar p r t0)]
                [:A [r r] 0.0 (if (> (- t1 t0) math/PI) 1 0) 1
                 (v/+polar p r t1)]]
               opts)))

;; Forces arcs to be clockwise from t0 to t1
(defn arc
  ([t0 t1] (arc (gv/vec2) 1.0 t0 t1))
  ([r t0 t1] (arc (gv/vec2) r t0 t1))
  ([p r t0 t1]
   (if (< t0 t1)
     (->Arc p r t0 t1)
     (->Arc p r t0 (+ t1 eq/TAU)))))
