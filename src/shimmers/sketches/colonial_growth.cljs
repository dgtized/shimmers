(ns shimmers.sketches.colonial-growth
  "Somewhere between diffusion limited aggregation and circle packing?"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.math.probability :as p]))

(defn child-tree [shapes]
  (reduce-kv (fn [t s]
               (if-let [parent (:parent s)]
                 (update t parent (fnil conj []) s)
                 t))
             {} shapes))

(defn in-bounds? [circle]
  (geom/contains-point? (rect/rect 0 0 (q/width) (q/height))
                        (:p circle)))

;; Sometimes generating inside of another circle?
(defn intersects [c1 c2]
  (when (or (geom/contains-point? c1 (:p c2))
            (geom/intersect-shape c1 c2))
    c2))

(defn border-circle [shapes]
  (let [inverted-tree (child-tree shapes)
        {:keys [p r] :as parent}
        (p/weighted-by (fn [s] (/ (:r s) (inc (count (get inverted-tree s)))))
                       shapes)

        angle (tm/random 0 tm/TWO_PI)
        radius (tm/random (max (* 0.6 r) 2) (* 1.1 r))
        center (->> (gv/vec2 (+ r radius 0.1) angle)
                    geom/as-cartesian
                    (tm/+ p))
        circle (assoc (gc/circle center radius) :parent parent)]
    (when (and (in-bounds? circle)
               (not (some (partial intersects circle) shapes)))
      circle)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(gc/circle (cq/rel-pos 0.5 0.5) (cq/rel-w 0.05))]})

(defn update-state [{:keys [shapes] :as state}]
  (if-let [new-circle (border-circle shapes)]
    (update state :shapes conj new-circle)
    state))

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r parent]} shapes
          :let [[x y] p]]
    (q/ellipse x y r r)
    (when parent
      (q/line p (:p parent)))))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch colonial-growth
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
