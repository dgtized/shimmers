(ns shimmers.sketches.amplification
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.circle :as gc]
   [thi.ng.math.core :as tm]
   [shimmers.math.vector :as v]
   [shimmers.math.equations :as eq]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn sketch-circle [pos r]
  (let [n (Math/ceil (* 6 (dr/circular-random)))]
    (for [_ (range n)]
      (gc/circle (tm/+ pos (dr/jitter 4.0)) r))))

(defn make-concentric [pos max-radius offsets]
  (mapcat (fn [o] (sketch-circle pos (* max-radius o)))
          offsets))

(defn shapes []
  (let [center (rv 0.5 0.5)]
    (concat (make-concentric center
                             (* 0.25 height)
                             (drop 1 (tm/norm-range 5)))
            (mapcat (fn [t]
                      (make-concentric (v/+polar center (* 0.3 height)
                                                 (+ (* 0.75 eq/TAU) (* eq/TAU t)))
                                       (* 0.15 height)
                                       (drop 1 (tm/norm-range 3))))
                    (drop 1 (tm/norm-range (dr/random-int 2 8)))))))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes)))

(sketch/definition amplification
  {:created-at "2023-03-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :amplification)
              "sketch-host"))
