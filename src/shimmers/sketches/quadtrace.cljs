(ns shimmers.sketches.quadtrace
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.math.core :as tm]))

(defn build-tree [bounds points]
  (reduce (fn [t {:keys [p] :as c}] (g/add-point t p c))
          (saq/circletree bounds) points))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.99)
        gen (fn [] (gc/circle (rp/sample-point-inside (g/scale-size bounds 0.9))
                             (dr/random-int 12 48)))
        points (repeatedly 1024 gen)]
    {:bounds bounds
     :points points
     :tree (build-tree bounds points)
     :t 0.0}))

(defn remove-point [tree]
  (let [p (rp/sample-point-inside (g/bounds tree))
        n (saq/nearest-neighbor-node tree p)]
    (g/delete-point tree (g/get-point n))))

(defn update-state [{:keys [t] :as state}]
  (let [rs (eq/unit-sin (+ (* 4 t) (Math/sin (* tm/PHI t))))
        p (tm/+ (cq/rel-vec 0.5 0.5) (v/polar (* (q/height) (+ 0.05 (* 0.4 rs)))
                                              t))
        cursor (gc/circle p (+ 2 (* 8 (eq/unit-sin (* 3 t)))))]
    (-> state
        (update :t + 0.01)
        (update :tree remove-point)
        (update :tree g/add-point p cursor))))

(defn breadth-seq [tree]
  (sort-by (fn [n] (:depth (meta n)))
           (tree-seq (fn [t] (not-empty (spatialtree/get-children t)))
                     (fn [t] (map #(vary-meta % assoc :depth (inc (:depth (meta t))))
                                 (remove nil? (spatialtree/get-children t))))
                     (vary-meta tree assoc :depth 0))))

(defn draw [{:keys [tree]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.66)
  (q/stroke 0.0 0.1)
  (q/fill 0.2 0.1)
  (doseq [node (breadth-seq tree)]
    (cq/rectangle (g/bounds node))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition quadtrace
  {:created-at "2024-02-06"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
