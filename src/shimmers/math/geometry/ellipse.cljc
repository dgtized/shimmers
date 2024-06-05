(ns shimmers.math.geometry.ellipse
  (:require
   [clojure.math :as math]
   [thi.ng.geom.core :as g :refer [*resolution*]]
   [thi.ng.geom.vector :as v :refer [vec2]]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [Circle2 Ellipse2 Polygon2 Rect2]])
   [thi.ng.math.core :as m :refer [TWO_PI]])
  #?(:clj
     (:import [thi.ng.geom.types Circle2 Ellipse2 Polygon2 Rect2])))

;; Start implementing some thi.ng.geom.core routines for Ellipse2
;; It's possible that the defrecord will need to account for axis angle though.

(defn ellipse
  ([] (Ellipse2. (vec2) 1.0 1.0))
  ([rx ry] (Ellipse2. (vec2) rx ry))
  ([p rx ry] (Ellipse2. (vec2 p) rx ry))
  ([x y rx ry] (Ellipse2. (vec2 x y) rx ry)))

(extend-type Ellipse2
  g/IArea
  (area [{:keys [rx ry]}] (* math/PI rx ry))

  g/IBounds
  (bounds
    [{:keys [p rx ry]}]
    (Rect2. (m/- p (vec2 rx ry)) (vec2 (* 2.0 rx) (* 2.0 ry))))
  (width [{:keys [rx]}] (* 2.0 rx))
  (height [{:keys [ry]}] (* 2.0 ry))
  (depth [_] 0)

  g/IBoundingCircle
  (bounding-circle [{:keys [p rx ry]}]
    (Circle2. p (max rx ry)))

  ;; https://math.stackexchange.com/a/76463/903738
  g/IBoundary
  (contains-point? [{[px py] :p :keys [rx ry]} [qx qy]]
    (<= (+ (/ (math/pow (- qx px) 2) (* rx rx))
          (/ (math/pow (- qy py) 2) (* ry ry)))
       1))

  g/ICenter
  (center
    ([{:keys [rx ry]}] (Ellipse2. (vec2) rx ry))
    ([{:keys [rx ry]} p] (Ellipse2. (vec2 p) rx ry)))
  (centroid [{:keys [p]}] p)

  ;; g/ICircumference
  ;; (circumfrence [])
  ;; Exact requires infinite series / calculus?

  g/IVertexAccess
  (vertices
    ([_] (g/vertices _ *resolution*))
    ([{:keys [p rx ry]} res]
     (->> (m/norm-range res)
          butlast
          (mapv (fn [x]
                  (let [t (* x TWO_PI)]
                    (m/+ p (vec2 (* rx (math/cos t))
                                 (* ry (math/sin t))))))))))
  g/IPolygonConvert
  (as-polygon
    ([_] (g/as-polygon _ *resolution*))
    ([_ res] (Polygon2. (vec (g/vertices _ res)))))

  g/IEdgeAccess
  (edges
    ([_] (g/edges _ *resolution*))
    ([_ res]
     (let [verts (g/vertices _ res)]
       (partition 2 1 (conj verts (first verts))))))

  g/ISample
  (point-at [{:keys [p rx ry]} t]
    (let [theta (* t TWO_PI)]
      (m/+ (vec2 (* rx (math/cos theta))
                 (* ry (math/sin theta)))
           p)))
  ;; This is not a uniform distribution of points on the circumfrence. From
  ;; https://codereview.stackexchange.com/questions/243590/generate-random-points-on-perimeter-of-ellipse
  ;; it sounds like calculating the circumfrence or total arc length, and
  ;; finding a random point on that and then backtracking to find it's theta and
  ;; project it back might work?
  (random-point
    [_] (g/point-at _ (m/random)))
  ;; First answer form https://stackoverflow.com/a/5529199/34450, but might not be uniform
  ;; as the theta's along a longer axis should have more samples?
  (random-point-inside [{:keys [p rx ry]}]
    (let [rho (math/sqrt (m/random))
          theta (* TWO_PI (m/random))]
      (m/+ (vec2 (* 2 rx rho (math/cos theta))
                 (* 2 ry rho (math/sin theta)))
           p))))
