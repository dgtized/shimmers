(ns shimmers.sketches.hatch-test
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils.intersect :as gisec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 800)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn example-rect [{[w _] :size :as cell}]
  (-> (rect/rect (/ w 1.5))
      (g/center (g/centroid cell))))

(defn examples []
  [(fn [cell]
     (let [rect (example-rect cell)]
       (into [rect]
             (clip/hatch-rectangle rect (dr/random 4 12) (dr/random-tau)
                                   [(dr/random) (dr/random)]))))
   (fn [{[w _] :size :as cell}]
     (let [rect (-> (rect/rect (/ w 1.5))
                    (g/center (g/centroid cell)))
           theta (dr/random-tau)]
       (into [(geometry/rotate-around-centroid rect theta)]
             (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                   (clip/hatch-rectangle rect (dr/random 4 12) (dr/random-tau)
                                         [(dr/random) (dr/random)])))))

   ;; hatching
   (fn [cell]
     (let [rect (example-rect cell)
           spacing (dr/random 6 16)
           theta (dr/random-tau)
           theta0 (dr/random-tau)
           theta1 (+ theta0 (dr/gaussian (* eq/TAU 0.25) 0.1))]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect spacing theta0
                                           [(dr/random) (dr/random)]))
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect spacing theta1
                                           [(dr/random) (dr/random)])))))

   ;; double with same theta
   (fn [cell]
     (let [rect (example-rect cell)
           theta (dr/random-tau)
           theta0 (dr/random-tau)]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect (dr/random 6 12) theta0
                                           [(dr/random) (dr/random)]))
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect (dr/random 6 12) theta0
                                           [(dr/random) (dr/random)])))))
   ;; double with close theta
   (fn [cell]
     (let [rect (example-rect cell)
           theta (dr/random-tau)
           theta0 (dr/random-tau)
           theta1 (dr/gaussian theta0 0.1)]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect (dr/random 6 12) theta0
                                           [(dr/random) (dr/random)]))
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect (dr/random 6 12) theta1
                                           [(dr/random) (dr/random)])))))
   ;; radial
   (fn [cell]
     (let [rect (example-rect cell)
           midpoint (g/unmap-point rect (gv/vec2 (dr/random 0.45 0.55)
                                                 (dr/random 0.45 0.55)))
           theta (dr/random-tau)]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (for [t (range 0 eq/TAU (/ eq/TAU (dr/random-int 15 40)))]
                 (geometry/rotate-around (gl/line2 midpoint (gisec/intersect-ray2-edges?
                                                             midpoint (v/polar 1.0 t)
                                                             (g/edges rect)))
                                         (g/centroid rect) theta)))))

   ;; seem line (not very pleasing)
   (fn [cell]
     (let [rect (example-rect cell)
           p (g/unmap-point rect (gv/vec2 (dr/random 0.35 0.65)
                                          (dr/random 0.35 0.65)))
           q (g/unmap-point rect (gv/vec2 (dr/random 0.35 0.65)
                                          (dr/random 0.35 0.65)))
           line (g/scale-size (gl/line2 p q) 1.2)
           theta (dr/random-tau)]
       (concat [(geometry/rotate-around-centroid rect theta)
                (geometry/rotate-around line (g/centroid rect) theta)]
               (for [t (tm/norm-range (dr/random-int 20 60))
                     :let [pt (g/point-at rect t)]]
                 (geometry/rotate-around (gl/line2 pt (g/closest-point line pt))
                                         (g/centroid rect) theta)))))

   ;; bubble hatching
   (fn [cell]
     (let [rect (example-rect cell)
           points (rp/poisson-disc-sampling rect (dr/random-int 200 300))
           rlen (/ (* 8 (g/width rect)) (count points))
           center (g/centroid rect)
           theta (dr/random-tau)
           base (dr/random-tau)]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (apply concat
                      (for [pt points
                            :let [d (g/dist pt (g/closest-point rect pt))
                                  angle0 (dr/gaussian base 0.05)
                                  angle1 (dr/gaussian (+ angle0 (* eq/TAU 0.25)) 0.1)
                                  len (fn [] (min d (dr/gaussian rlen (/ rlen 8.0))))]
                            :when (> d (* 0.5 rlen))]
                        [(geometry/rotate-around (gl/line2 (v/-polar pt (len) angle0)
                                                           (v/+polar pt (len) angle0))
                                                 center theta)
                         (geometry/rotate-around (gl/line2 (v/-polar pt (len) angle1)
                                                           (v/+polar pt (len) angle1))
                                                 center theta)])))))

   (fn [cell]
     (let [rect (example-rect cell)
           points (rp/poisson-disc-sampling rect (dr/random-int 200 300))
           rlen (/ (* 6 (g/width rect)) (count points))
           center (g/centroid rect)
           theta (dr/random-tau)]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (for [pt points
                     :let [d (g/dist pt (g/closest-point rect pt))
                           len (fn [] (min d (dr/gaussian rlen (/ rlen 8.0))))]
                     :when (> d (* 0.5 rlen))]
                 (geometry/rotate-around (gc/circle pt (len)) center theta)))))])

(defn shapes [bounds examples]
  (for [[cell example] (map vector (g/subdivide bounds {:num 3})
                            examples)]
    (csvg/group {}
      (example cell))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes (csvg/screen width height)
            (examples))))

(sketch/definition hatch-test
  {:created-at "2025-01-22"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
