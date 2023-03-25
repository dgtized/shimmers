(ns shimmers.sketches.amplification
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
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

(defn distance-to-edge [bounds p]
  (g/dist p (g/closest-point bounds p)))

(defn sketch-circle [pos r]
  (let [n (Math/ceil (* 6 (dr/circular-random)))]
    (for [_ (range n)]
      (gc/circle (tm/+ pos (dr/jitter 4.0)) r))))

(defn make-concentric [pos max-radius offsets]
  (mapcat (fn [o] (sketch-circle pos (* max-radius o)))
          offsets))

(defn shapes [bounds]
  (let [center (rv (dr/random 0.25 0.75) (dr/random 0.35 0.65))
        edge-dist (distance-to-edge bounds center)]
    (concat (make-concentric center
                             (* 0.66 edge-dist)
                             (drop 1 (tm/norm-range 5)))
            (mapcat (fn [t]
                      (let [proj (v/+polar center (* 0.3 height)
                                           (+ (* 0.75 eq/TAU) (* eq/TAU t)))]
                        (if (g/contains-point? bounds proj)
                          (make-concentric proj
                                           (* 0.33 edge-dist)
                                           (drop 1 (tm/norm-range 3)))
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
