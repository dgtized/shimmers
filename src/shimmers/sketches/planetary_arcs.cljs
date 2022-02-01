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
    (csvg/path [[:M start] [:A [r r] 0 large-arc sweep end]]
               {:stroke (if (= sweep 1) "blue" "red")})))

(defn planet [p radius inputs]
  (->> inputs
       (sort-by first)
       cs/triplet-cycle
       (mapcat
        (fn [[[angle0 _] [angle n] [angle1 _]]]
          #_(println [angle0 angle angle1])
          (let [ranges (cs/midsection (dr/var-range (inc n)))
                radial0 (sm/radial-distance angle0 angle)
                radial1 (sm/radial-distance angle angle1)]
            (into [(gc/circle p 1)
                   (gl/line2 (tm/+ p (v/polar (* radius (first ranges)) angle))
                             (tm/+ p (v/polar (* radius (last ranges)) angle)))]
                  (for [arc ranges
                        :let [ra (* radius arc)
                              theta (cond (and (tm/delta= angle0 angle) (tm/delta= angle angle1))
                                          (dr/random (* -0.9 eq/TAU)
                                                     (* 0.9 eq/TAU))
                                          (tm/delta= angle0 angle1)
                                          (dr/random (* -0.9 radial0)
                                                     (* 0.9 radial1))
                                          :else
                                          (dr/random (* -0.9 radial0)
                                                     (* 0.9 radial1)))]]
                    (relative-arc p ra angle theta))))))))

;; handle loop around for the long direction
(comment (sm/radial-distance (* 0.0 eq/TAU) (* 0.75 eq/TAU)))

(defn shapes []
  (concat (planet (rv 0.25 0.5) (* height 0.3) [[(* 0.0 eq/TAU) 7] [(* 0.25 eq/TAU) 7]])
          (planet (rv 0.75 0.5) (* height 0.3) [[(* 0.0 eq/TAU) 7] [(* 0.5 eq/TAU) 7] [(* 0.75 eq/TAU) 11]])))

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
