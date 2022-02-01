(ns shimmers.sketches.planetary-arcs
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.line :as gl]
   [shimmers.math.vector :as v]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn arc-path [p r start-angle end-angle]
  (let [start (tm/+ p (v/polar r end-angle))
        end (tm/+ p (v/polar r start-angle))
        large-arc (if (<= (- end-angle start-angle) Math/PI) 0 1)]
    (csvg/path [[:M start] [:A [r r] 0 large-arc 0 end]])))

(defn planet [p radius angle n]
  (into [(gl/line2 p (tm/+ p (v/polar radius angle)))]
        (for [arc (dr/var-range n)
              :let [ra (* radius arc)
                    theta (dr/random (* 0.3 eq/TAU) (* 0.9 eq/TAU))]]
          (arc-path p ra angle theta))))

(defn shapes []
  (concat (planet (rv 0.5 0.5) (* height 0.3) 0 5)))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition planetary-arcs
  {:created-at "2022-01-31"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :planetary-arcs)
              "sketch-host"))
