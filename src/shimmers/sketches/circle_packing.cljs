(ns shimmers.sketches.circle-packing
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.rect :as rect]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 10)
  {:quadtree (spatialtree/quadtree 0 0 (q/width) (q/height))
   :boundary (rect/rect 0 0 (q/width) (q/height))
   :circles []})

(defn contains-entity? [rect {:keys [p r]}]
  (let [closest (geom/closest-point rect p)]
    (< (* r r) (geom/dist-squared closest p))))

(defn add-circle [quadtree boundary]
  (let [center (gv/vec2 (q/random (q/width)) (q/random (q/height)))
        r 2
        circle (gc/circle center r)
        near (spatialtree/select-with-circle quadtree center (* r 10))]
    (println circle)
    (when (and (contains-entity? boundary circle)
               (not (some (partial geom/intersect-shape circle) near)))
      circle)))

(defn grow [quadtree boundary circle]
  (let [growth (geom/scale-size circle 1.025)
        near (spatialtree/select-with-circle quadtree (:p growth) (* (:r growth) 10))]
    (if (and (contains-entity? boundary growth)
             (not (some (partial geom/intersect-shape growth) near)))
      growth
      circle)))

(defn update-state [{:keys [boundary quadtree circles] :as state}]
  (let [growth (map (partial grow quadtree boundary) circles)
        s' (assoc state :circles growth)]
    (if-let [circle (add-circle quadtree boundary)]
      (-> s'
          (update :circles conj circle)
          (update :quadtree geom/add-point (:p circle) circle))
      s')))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (doseq [{:keys [p r]} circles]
    (q/ellipse (:x p) (:y p) r r)))

(defn ^:export run-sketch []
  (q/defsketch circle-packing
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
