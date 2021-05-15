(ns shimmers.sketches.colonial-growth
  "Somewhere between diffusion limited aggregation and circle packing?"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn child-tree [shapes]
  (reduce-kv (fn [t s]
               (if-let [parent (:parent s)]
                 (update t parent (fnil conj []) s)
                 t))
             {} shapes))

(defn in-bounds? [circle]
  (geom/contains-point? (rect/rect 0 0 (q/width) (q/height))
                        (:p circle)))

(defn intersects [c1 c2]
  (when (or (geom/contains-point? c2 (:p c1))
            (geom/intersect-shape c1 c2))
    c2))

(defn border-circle [shapes]
  (let [inverted-tree (child-tree shapes)
        {:keys [p r] :as parent}
        (p/weighted-by (fn [s] (/ (:r s) (inc (count (get inverted-tree s)))))
                       shapes)

        angle (tm/random 0 tm/TWO_PI)
        radius (tm/random (max (* 0.6 r) 2)
                          (min (* 1.15 r) (cq/rel-w 0.04)))
        center (->> (gv/vec2 (+ r radius 0.1) angle)
                    geom/as-cartesian
                    (tm/+ p))
        circle (assoc (gc/circle center radius) :parent parent)]
    (when (and (in-bounds? circle)
               (not (some (partial intersects circle) shapes)))
      (assoc circle :color (update (:color parent) 2 * 1.07)))))

(defn init-source []
  (assoc (gc/circle (cq/rel-pos (tm/random 0.2 0.8) (tm/random 0.2 0.8))
                    (cq/rel-w 0.05))
         :color [(rand-nth [0.0 0.4 0.5 0.6 0.85]) 0.35 0.4 1.0]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(init-source)]})

(defn update-state [{:keys [shapes] :as state}]
  (if-let [new-circle (->> #(border-circle shapes)
                           (repeatedly 8)
                           (drop-while nil?)
                           first)]
    (update state :shapes conj new-circle)
    state))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r parent color]} shapes
          :let [[x y] p]]
    (cq/color-if q/fill color)
    (q/ellipse x y r r)
    #_(when parent (q/line p (:p parent)))))

(defn ^:export run-sketch []
  ;; 2021
  (q/defsketch colonial-growth
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
