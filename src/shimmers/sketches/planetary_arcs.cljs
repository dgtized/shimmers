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

(defn relative-arc [p r start-angle d-angle]
  (let [end-angle (+ start-angle d-angle)
        start (tm/+ p (v/polar r end-angle))
        end (tm/+ p (v/polar r start-angle))
        large-arc (if (<= (Math/abs (- end-angle start-angle)) Math/PI) 0 1)
        sweep (if (> d-angle 0) 0 1)]
    {:start start
     :end end
     :large-arc large-arc
     :sweep sweep}))

(defn planet [p radius inputs]
  (->> inputs
       (sort-by first)
       cs/triplet-cycle
       (mapcat
        (fn [[[angle0 _] [angle n] [angle1 _]]]
          (let [ranges (cs/midsection (dr/var-range (inc n)))
                delta0 (sm/radial-distance angle0 angle)
                radial0 (if (< angle0 angle) delta0 (- eq/TAU delta0))
                delta1 (sm/radial-distance angle angle1)
                radial1 (if (< angle angle1) delta1 (- eq/TAU delta1))]
            (into [(gc/circle p 1)
                   (gl/line2 (tm/+ p (v/polar (* radius (first ranges)) angle))
                             (tm/+ p (v/polar (* radius (last ranges)) angle)))]
                  (for [arc ranges
                        :let [ra (* radius arc)
                              theta (dr/random (* -0.9 radial0) (* 0.9 radial1))
                              {:keys [start end large-arc sweep]} (relative-arc p ra angle theta)]]
                    (csvg/path [[:M start] [:A [ra ra] 0 large-arc sweep end]]
                               {:stroke (if (= sweep 1) "blue" "red")}))))))))

;; handle loop around for the long direction
(comment (sm/radial-distance (* 0.0 eq/TAU) (* 0.75 eq/TAU)))

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
