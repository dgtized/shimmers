(ns shimmers.sketches.decorative-tiles
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; something is wrong with the facing signs
(defn connections [shape dir]
  (for [[a b] (g/edges shape)
        :let [mid (tm/mix a b 0.5)
              am (tm/- (tm/- mid (g/centroid shape)))]
        :when (or (not dir)
                  (> (sm/radial-distance (g/heading am) (g/heading dir)) 0.1))]
    [shape mid]))

(defn connection-pt [shape dir]
  (some (fn [[a b]]
          (let [mid (tm/mix a b 0.5)
                am (tm/- (tm/- mid (g/centroid shape)))]
            (when (< (sm/radial-distance (g/heading am) (g/heading dir)) 0.1)
              am)))
        (g/edges shape)))

(defn m-square [size]
  (g/center (rect/rect size)))

(defn m-rectangle [angle size]
  (g/rotate (g/center (rect/rect 0 0 size (* tm/PHI size)))
            angle))

;; https://en.wikipedia.org/wiki/Regular_polygon#Circumradius
(defn n-gon
  "Construct a regular polygon with n faces from a circle, and rotate to a flat
  edge at angle zero."
  [n]
  (fn [size]
    (let [s (-> (gc/circle (/ size (* 2 (Math/sin (/ Math/PI n)))))
                (g/as-polygon n))]
      (if (even? n)
        (g/rotate s (/ Math/PI n))
        s))))

(defn gen-shape []
  (dr/weighted [[(n-gon 3) 4]
                [m-square 4]
                [(partial m-rectangle 0) 3]
                [(partial m-rectangle tm/HALF_PI) 2]
                [(n-gon 5) 2]
                [(n-gon 6) 1]
                [(n-gon 7) 1]
                [(n-gon 8) 1]]))

(defn layers [seed size n]
  (loop [i n layer [seed] shapes [seed]]
    (if (= i 0)
      shapes
      (let [connects
            (->> layer
                 (mapcat
                  (fn [s]
                    (let [dir (when-let [parent (:parent s)]
                                (tm/- (g/centroid s) (g/centroid parent)))]
                      (connections s dir)))))
            mult (dr/weighted {1 5
                               0.5 1
                               (/ 1 tm/PHI) 1})
            m-shape (gen-shape)]
        (recur
         (dec i)
         (for [[shape connect] connects]
           (let [dir (tm/- connect (g/centroid shape))
                 angle (g/heading dir)
                 addition (g/rotate (m-shape (* size mult)) angle)
                 connect-pt (connection-pt addition dir)]
             (-> addition
                 (g/translate (tm/+ connect (tm/* connect-pt 1.1)))
                 (assoc :parent shape))))
         (into shapes layer))))))

(defn shapes []
  (let [n-layers (dr/random-int 3 7)
        size 40]
    (layers (g/translate ((gen-shape) size) (rv 0.5 0.5))
            size
            n-layers)))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill-opacity "5%"
              :fill "black"
              :stroke-width 1.0}
     (shapes))))

(sketch/definition decorative-tiles
  {:created-at "2023-01-20"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :decorative-tiles)
              "sketch-host"))
