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

(defn spiral-points [center dr' rotations]
  (for [t (tm/norm-range (int (* 7 rotations)))
        :let [theta (* eq/TAU rotations t)]]
    (tm/+ center (v/polar (* (* theta dr') (/ theta eq/TAU)) theta))))

(defn perpindiculars [center dr' rotations width]
  (for [t (dr/density-range 0.0001 0.0005)
        :let [theta (* eq/TAU rotations (Math/sqrt t))
              dr (* theta dr')
              r (* dr (/ theta eq/TAU))]]
    (gl/line2 (tm/+ center (v/polar (- r (* dr width)) theta))
              (tm/+ center (v/polar (+ r (* dr width)) theta)))))

(defn draw-spiral [spiral]
  (svg/group {:stroke-width 0.3}
             (->> (for [p spiral] [:T p])
                  (into [[:M (first spiral)]])
                  (csvg/path))))

;; can we make std-dev smoothly transition across both spirals?
(defn draw-perps [perps std-dev]
  (svg/group {} (for [[i line] (map-indexed vector perps)]
                  (let [c (g/centroid line)]
                    (-> line
                        g/center
                        (g/rotate (dr/gaussian (* 3 eq/TAU (/ i (count perps))) std-dev))
                        (g/translate c))))))

(defn shapes []
  (let [spiral (spiral-points (rv 0.2428 0.525) 0.25 11)
        spiral2 (spiral-points (rv 0.74 0.525) 0.25 11.5)
        perps (perpindiculars (rv 0.2428 0.525) 0.25 11 0.75)
        perps2 (perpindiculars (rv 0.74 0.525) 0.25 11.5 0.75)]
    (svg/group {}
               (draw-spiral spiral)
               (draw-spiral spiral2)
               (draw-perps perps 0.05)
               (draw-perps perps2 0.20))))

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
