(ns shimmers.sketches.colonial-growth
  "Somewhere between diffusion limited aggregation and circle packing?"
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.circle-packing :as pack]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.math.core :as tm]))

(defn child-tree [shapes]
  (reduce-kv (fn [t s]
               (if-let [parent (:parent s)]
                 (update t parent (fnil conj []) s)
                 t))
             {} shapes))

(defn border-circle [shapes]
  (let [inverted-tree (child-tree shapes)
        {:keys [p r] :as parent}
        (dr/weighted-by (fn [s] (/ (:r s) (inc (count (get inverted-tree s)))))
                        shapes)

        angle (dr/random tm/TWO_PI)
        radius (dr/random (max (* 0.6 r) 2)
                          (min (* 1.15 r) (cq/rel-w 0.04)))
        center (v/+polar p (+ r radius 0.1) angle)
        circle (assoc (gc/circle center radius) :parent parent)]
    (assoc circle :color (update (:color parent) 2 * 1.07))))

;; Improve palette selection?
(defn make-source []
  (assoc (gc/circle (cq/rel-pos (dr/random 0.2 0.8) (dr/random 0.2 0.8))
                    (cq/rel-w 0.05))
         :color [(dr/rand-nth [0.0 0.4 0.5 0.6 0.85]) 0.35 0.4 1.0]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)
        [circles tree]
        (->> {:bounds bounds :gen-circle make-source}
             (pack/pack-candidates (saq/circletree bounds) (dr/random-int 1 5)))]
    {:shapes circles
     :circletree tree}))

(defn update-state [{:keys [shapes circletree] :as state}]
  (let [rules {:bounds (cq/screen-rect)
               :gen-circle (partial border-circle shapes)}
        [circles tree] (pack/pack-candidates circletree 10 rules)]
    (assoc state
           :shapes (into shapes circles)
           :tree tree)))

(defonce ui-state (ctrl/state {:show-parent false}))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (let [{:keys [show-parent]} @ui-state]
    (doseq [{:keys [p r parent color]} shapes]
      (cq/color-if q/fill color)
      (cq/circle p r)
      (when (and show-parent parent)
        (q/line p (:p parent))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ctrl/checkbox ui-state "Show Parent" [:show-parent]]])

(sketch/definition colonial-growth
  {:created-at "2021-05-14"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))
