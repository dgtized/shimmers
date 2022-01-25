(ns shimmers.sketches.hairy-spiral
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn spiral-points [center dr dtheta steps]
  (for [theta (range 0 (* steps dtheta) dtheta)]
    (tm/+ center (v/polar (* dr (/ theta eq/TAU)) theta))))

(defn perpindiculars [center dr dtheta steps]
  (for [t (dr/density-range 0.0002 0.0008)
        :let [theta (+ (* 0.1 eq/TAU) (* 0.99 dtheta steps (Math/sqrt t)))
              r (* dr (/ theta eq/TAU))]]
    (gl/line2 (tm/+ center (v/polar (- r (* dr 0.4)) theta))
              (tm/+ center (v/polar (+ r (* dr 0.4)) theta)))))

(defn shapes []
  (let [spiral (spiral-points (rv 0.5 0.5) 24 0.3 256)
        perps (perpindiculars (rv 0.5 0.5) 24 0.3 256)]
    (svg/group {}
               (svg/group {:stroke-width 0.3}
                          (->> (for [p spiral] [:T p])
                               (into [[:M (first spiral)]])
                               (csvg/path)))
               (svg/group {} (for [[i line] (map-indexed vector perps)]
                               (let [c (g/centroid line)]
                                 (-> line
                                     g/center
                                     (g/rotate (* 0.2 (dr/gaussian (/ i (count perps)) 1)))
                                     (g/translate c))))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.8}
            (shapes)))

(sketch/definition hairy-spiral
  {:created-at "2022-01-25"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :hairy-spiral)
              "sketch-host"))
