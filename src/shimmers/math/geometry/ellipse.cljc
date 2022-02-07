(ns shimmers.math.geometry.ellipse
  (:require
   [thi.ng.geom.core :as g :refer [*resolution*]]
   [thi.ng.geom.vector :as v :refer [vec2]]
   #?(:clj [thi.ng.geom.types]
      :cljs [thi.ng.geom.types :refer [Circle2 Ellipse2 Rect2]])
   [thi.ng.math.core :as m :refer [PI TWO_PI *eps*]])
  #?(:clj
     (:import [thi.ng.geom.types Circle2 Ellipse2 Rect2])))

;; Start implementing some thi.ng.geom.core routines for Ellipse2
;; It's possible that the defrecord will need to account for axis angle though.

(defn ellipse
  ([] (Ellipse2. (vec2) 1.0 1.0))
  ([rx ry] (Ellipse2. (vec2) rx ry))
  ([p rx ry] (Ellipse2. (vec2 p) rx ry))
  ([x y rx ry] (Ellipse2. (vec2 x y) rx ry)))

(extend-type Ellipse2
  g/IArea
  (area [{:keys [rx ry]}] (* PI rx ry))

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

  ;; g/IBoundary
  ;; contains-point?

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
                    (m/+ p (vec2 (* rx (Math/cos t))
                                 (* ry (Math/sin t))))))))))

  g/IEdgeAccess
  (edges
    ([_] (g/edges _ *resolution*))
    ([_ res]
     (let [verts (g/vertices _ res)]
       (partition 2 1 (conj verts (first verts)))))))

