(ns shimmers.sketches.colonial-growth
  "Somewhere between diffusion limited aggregation and circle packing?"
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn child-tree [shapes]
  (reduce-kv (fn [t s]
               (if-let [parent (:parent s)]
                 (update t parent (fnil conj []) s)
                 t))
             {} shapes))

(defn in-bounds? [circle]
  (g/contains-point? (cq/screen-rect) (:p circle)))

(defn border-circle [shapes]
  (let [inverted-tree (child-tree shapes)
        {:keys [p r] :as parent}
        (dr/weighted-by (fn [s] (/ (:r s) (inc (count (get inverted-tree s)))))
                        shapes)

        angle (dr/random tm/TWO_PI)
        radius (dr/random (max (* 0.6 r) 2)
                          (min (* 1.15 r) (cq/rel-w 0.04)))
        center (->> (gv/vec2 (+ r radius 0.1) angle)
                    g/as-cartesian
                    (tm/+ p))
        circle (assoc (gc/circle center radius) :parent parent)]
    (when (and (in-bounds? circle)
               (not (some (partial geometry/circles-overlap? circle) shapes)))
      (assoc circle :color (update (:color parent) 2 * 1.07)))))

;; Improve palette selection?
(defn make-source []
  (assoc (gc/circle (cq/rel-pos (dr/random 0.2 0.8) (dr/random 0.2 0.8))
                    (cq/rel-w 0.05))
         :color [(dr/rand-nth [0.0 0.4 0.5 0.6 0.85]) 0.35 0.4 1.0]))

(defn add-non-intersecting [shapes]
  (let [c (make-source)]
    (if (some (partial geometry/circles-overlap? c) shapes)
      shapes
      (conj shapes c))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes (->> [(make-source)]
                (iterate add-non-intersecting)
                (take (dr/random-int 1 4))
                flatten
                vec)})

(defn update-state [{:keys [shapes] :as state}]
  (if-let [new-circle (->> #(border-circle shapes)
                           (repeatedly 8)
                           (drop-while nil?)
                           first)]
    (update state :shapes conj new-circle)
    state))

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

(defn ui-controls []
  [:div (ctrl/checkbox ui-state "Show Parent" [:show-parent])])

(sketch/defquil colonial-growth
  :created-at "2021-05-14"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :tags #{:deterministic}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
