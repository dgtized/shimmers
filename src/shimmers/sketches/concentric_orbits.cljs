(ns shimmers.sketches.concentric-orbits
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

(defn max-radius [r n]
  (/ r (+ Math/E (Math/sin (/ eq/TAU n)))))

(defn orbit [{:keys [p r]} n phase]
  (for [t (butlast (tm/norm-range n))]
    (let [radius (max-radius r n)]
      (gc/circle (v/+polar p
                           (- r radius)
                           (+ phase (* eq/TAU t)))
                 radius))))

(defn shapes []
  (let [prime (gc/circle (rv 0.5 0.5) (* 0.48 height))]
    (loop [orbits [] depth 0 layer [prime]]
      (if (> depth 3)
        (into orbits layer)
        (recur (into orbits layer)
               (inc depth)
               (mapcat (fn [parent]
                         (orbit parent (dr/random-int 2 7) (dr/random-tau)))
                       layer))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :concentric-orbits]
     [:div.readable-width]]))

(sketch/definition concentric-orbits
  {:created-at "2024-03-11"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
