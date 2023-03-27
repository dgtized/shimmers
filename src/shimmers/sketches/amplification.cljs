(ns shimmers.sketches.amplification
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn distance-to-edge [bounds p]
  (g/dist p (g/closest-point bounds p)))

(defn skip-line [a b]
  (->> (dr/random-int 12 48)
       (lines/segmented (gl/line2 a b))
       (drop 1)
       (keep-indexed (fn [i x] (when (= 0 (mod i 3)) x)))))

(defn arc-segment [pos r t0 t1]
  (let [src (v/+polar pos r t0)
        dest (v/+polar pos r t1)]
    (csvg/path [[:M src]
                [:A [r r] 0.0
                 0
                 (if (> (Math/abs (- t1 t0)) Math/PI) 0 1)
                 dest]])))

(defn sketch-circle [pos r]
  (let [n (Math/ceil (* 6 (dr/circular-random)))]
    (mapcat (fn [_]
              [(gc/circle (tm/+ pos (dr/jitter 4.0)) r)
               (arc-segment pos (* r (dr/gaussian 1.0 0.01)) 0 (* 0.66 eq/TAU))])
            (range n))))

(defn make-concentric [pos max-radius offsets]
  (mapcat (fn [o] (sketch-circle pos (* max-radius o)))
          offsets))

(defn shapes [bounds]
  (let [center (rv (dr/random 0.25 0.75) (dr/random 0.35 0.65))
        close-edge-point (g/closest-point bounds center)
        edge-dist (g/dist center close-edge-point)]
    (concat (make-concentric center
                             (* 0.66 edge-dist)
                             (drop 1 (tm/norm-range 5)))
            (mapcat (fn [t]
                      (let [direction (dr/gaussian (+ (* 0.75 eq/TAU) (* eq/TAU t)) 0.2)
                            proj (v/+polar center
                                           (+ (* (- 1 (eq/cos-similarity (tm/- close-edge-point center)
                                                                         (v/polar 1 direction)))
                                                 (* 0.5 edge-dist))
                                              (* 0.6 edge-dist))
                                           direction)
                            proj-edge-dist (distance-to-edge bounds proj)]
                        (if (and (g/contains-point? bounds proj)
                                 (> proj-edge-dist 25))
                          (concat (make-concentric proj
                                                   (* 0.4 proj-edge-dist)
                                                   (drop 1 (tm/norm-range 3)))
                                  (skip-line center proj))
                          [])))
                    (drop 1 (tm/norm-range (dr/random-int 2 8)))))))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height))))

(sketch/definition amplification
  {:created-at "2023-03-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :amplification)
              "sketch-host"))
