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

(defn spiral-points [center dr' dtheta rotations]
  (for [theta (range 0 (* eq/TAU rotations) dtheta)]
    (tm/+ center (v/polar (* (* theta dr') (/ theta eq/TAU)) theta))))

(defn perpindiculars [center dr' rotations]
  (for [t (dr/density-range 0.0002 0.0008)
        :let [theta (* eq/TAU rotations (Math/sqrt t))
              dr (* theta dr')
              r (* dr (/ theta eq/TAU))]]
    (gl/line2 (tm/+ center (v/polar (- r (* dr 0.5)) theta))
              (tm/+ center (v/polar (+ r (* dr 0.5)) theta)))))

(defn shapes []
  (let [spiral (spiral-points (rv 0.5 0.5) 0.25 0.3 14.5)
        perps (perpindiculars (rv 0.5 0.5) 0.25 14.5)]
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

;; TODO: add automatic timing to csvg/svg?
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
