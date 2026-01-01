(ns shimmers.sketches.connect-and-fill
  (:require
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.svg.core :as svg]
   [shimmers.common.string :as scs]
   [thi.ng.color.core :as col]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn displace [bounds p]
  (let [radius (* 0.5 (min width height))]
    (->>
     (fn [] (v/+polar p (dr/random (* 0.15 radius) (* 0.75 radius))
                     (dr/random-tau)))
     repeatedly
     (some
      (fn [p]
        (when (g/contains-point? bounds p)
          p))))))


(defn point-triplets [pts]
  (partition 3 1 (concat pts (take 2 pts))))

(comment
  (point-triplets (vec (range 4))))

(defn acceptable? [pts]
  (every?
   (fn [[a b c]]
     (let [angle (poly-detect/small-angle-between (tm/- a b) (tm/- c b))]
       (when (<= angle (* eq/TAU 0.05)) (println [a b c] angle))
       (> angle (* eq/TAU 0.05))))
   (point-triplets pts)))

(defn points [bounds n]
  (->>
   (fn []
     (take n
           (iterate (fn [p] (displace (g/scale-size bounds 0.95) p))
                    (rp/sample-point-inside (g/scale-size bounds 0.75)))))
   repeatedly
   (some
    (fn [pts] (when (acceptable? (vec pts)) pts)))))

(defn debug-points [pts]
  (mapv (fn [[a b c]]
          (csvg/group {}
            (vary-meta (gc/circle b 3.0)
                       assoc
                       :fill (let [angle (poly-detect/small-angle-between (tm/- a b) (tm/- c b))]
                               (if (< angle (* eq/TAU 0.05))
                                 "red" "green")))
            (svg/text (tm/+ b (gv/vec2 5.0 0.0))
                      (apply scs/cl-format "[~0,1f, ~0,1f]" b))))
        (point-triplets pts)))

(defn shapes [bounds]
  (let [pts (vec (points bounds 10))]
    (concat #_(debug-points pts)
            (into
             [(gp/polygon2 pts)]
             (repeatedly 4 #(vary-meta (g/translate (gp/polygon2 pts)
                                                    (gv/vec2 (* (dr/random-sign) (dr/random 2 26))
                                                             (* (dr/random-sign) (dr/random 2 26))))
                                       assoc :fill (col/as-css (col/hsla 0.55 0.5 0.5 0.05))))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes (csvg/screen width height))))

(sketch/definition connect-and-fill
  {:created-at "2026-01-01"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
