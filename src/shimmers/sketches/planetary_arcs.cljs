(ns shimmers.sketches.planetary-arcs
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn clockwise-arc [p r start-angle end-angle]
  (let [start (tm/+ p (v/polar r end-angle))
        end (tm/+ p (v/polar r start-angle))
        large-arc (if (<= (Math/abs (- end-angle start-angle)) Math/PI) 0 1)]
    (csvg/path [[:M start] [:A [r r] 0 large-arc 0 end]])))

(defn counter-clockwise-arc [p r start-angle end-angle]
  (let [start (tm/+ p (v/polar r end-angle))
        end (tm/+ p (v/polar r start-angle))
        large-arc (if (<= (Math/abs (- end-angle start-angle)) Math/PI) 1 0)]
    (csvg/path [[:M start] [:A [r r] 0 large-arc 1 end]])))

;; FIXME: address issues with arcs when starting angle is between 0.25 and 1.0
;; of eq/TAU
(defn planet [p radius angle n]
  (let [ranges (rest (dr/var-range n))]
    (into [(gc/circle p 1)
           (gl/line2 (tm/+ p (v/polar (* radius (first ranges)) angle))
                     (tm/+ p (v/polar (* radius (last ranges)) angle)))]
          (for [arc ranges
                :let [ra (* radius arc)
                      theta (dr/random (* 0.3 eq/TAU) (* 0.9 eq/TAU))]]
            ((dr/rand-nth [clockwise-arc counter-clockwise-arc]) p ra angle theta)))))

(defn shapes []
  (concat (planet (rv 0.25 0.5) (* height 0.3) (* 0.0 eq/TAU) 7)
          (planet (rv 0.75 0.5) (* height 0.3) (* 0.0 eq/TAU) 17)))

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
