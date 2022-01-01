(ns shimmers.sketches.hexflare
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn rp [r theta]
  (v/polar (* r 0.5 height) theta))

(defn scale-dist [center factor]
  (fn [s]
    (let [pct (- 1.0 (/ (g/dist (g/centroid s) center) (* factor height)))]
      (g/scale-size s pct))))

(defn shapes []
  (let [r (* height 0.05)
        a (rv 0.0 0.9)
        mid-face (g/as-cartesian (gv/vec2 r (- tm/TWO_PI (* (/ 1 12) tm/TWO_PI))))
        b (gv/vec2 width (clip/project-y a (tm/+ a mid-face) width))
        line1 (gl/line2 a b)
        dist (tm/mag line1)
        hexes (for [t (dr/density-range (/ (* 1.1 (Math/sqrt 3) r) dist)
                                        (/ (* 4 r) dist))]
                (g/as-polygon (gc/circle (g/point-at line1 t) r) 6))
        center (apply gp/polygon2 (gp/inset-polygon (g/vertices (cs/middle hexes)) 5))
        centroid (g/centroid center)
        right-down (g/as-cartesian (gv/vec2 r (/ tm/TWO_PI 12)))
        line2 (clip/clip-line (rect/rect 0 0 width height)
                              (tm/- centroid (tm/* right-down width))
                              (tm/+ centroid (tm/* right-down width)))
        hexes2 (->> (for [t (dr/density-range (/ (* 1.1 (Math/sqrt 3) r) dist)
                                              (/ (* 4 r) dist))]
                      (g/as-polygon (gc/circle (g/point-at line2 t) r) 6))
                    (remove #(< (g/dist centroid (g/centroid %)) (* 2 r))))
        ]
    (concat [line1 line2 center]
            (mapv (scale-dist centroid 1.3) hexes)
            (mapv (scale-dist centroid 1.8) hexes2))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition hexflare
  {:created-at "2021-12-30"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :hexflare)
              "sketch-host"))
