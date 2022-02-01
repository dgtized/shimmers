(ns shimmers.sketches.planetary-arcs
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
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

(defn relative-arc
  "Calculate arc flags for an SVG path from a start-angle to a relative theta.

  FIXME: Doesn't handle completing a circle if |dtheta| >= 𝜏."
  [p r start-angle dtheta]
  (let [end-angle (+ start-angle dtheta)
        start (tm/+ p (v/polar r end-angle))
        end (tm/+ p (v/polar r start-angle))
        large-arc (if (<= (Math/abs (- end-angle start-angle)) Math/PI) 0 1)
        sweep (if (> dtheta 0) 0 1)]
    {:start start
     :end end
     :large-arc large-arc
     :sweep sweep}))

(defn planet [p radius inputs]
  (let [arcs (reduce + (map second inputs))
        all-ranges (dr/shuffle (rest (dr/var-range (+ arcs 1))))
        ordered-inputs (sort-by first inputs)]
    (into [(gc/circle p 1)]
          (mapcat
           (fn [[angle0 angle angle1] ranges]
             (let [ranges (sort ranges)
                   delta0 (sm/radial-distance angle0 angle)
                   radial0 (if (< angle0 angle) delta0 (- eq/TAU delta0))
                   delta1 (sm/radial-distance angle angle1)
                   radial1 (if (< angle angle1) delta1 (- eq/TAU delta1))]
               (into [(gl/line2 (tm/+ p (v/polar (* radius (first ranges)) angle))
                                (tm/+ p (v/polar (* radius (last ranges)) angle)))]
                     (for [arc ranges
                           :let [ra (* radius arc)
                                 theta (dr/random (* -0.9 radial0) (* 0.9 radial1))
                                 {:keys [start end large-arc sweep]} (relative-arc p ra angle theta)]]
                       (csvg/path [[:M start] [:A [ra ra] 0 large-arc sweep end]]
                                  {:stroke (if (= sweep 1) "blue" "red")})))))
           (cs/triplet-cycle (map first ordered-inputs))
           (cs/partition-chunks (map second ordered-inputs) all-ranges)))))

;; TODO: generate a MST and map planets to each of the points
(defn shapes []
  (concat (planet (rv 0.25 0.5) (* height 0.3) [[(* 0.0 eq/TAU) 7] [(* 0.25 eq/TAU) 7]])
          (planet (rv 0.75 0.5) (* height 0.3) [[(* 0.33 eq/TAU) 7] [(* 0.5 eq/TAU) 7] [(* 0.66 eq/TAU) 11]])))

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
