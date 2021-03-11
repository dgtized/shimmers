(ns shimmers.sketches.circle-packing
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.vector :as gv]))

(defn random-color []
  [(rand)
   (q/random 0.3 0.9)
   (q/random 0.3 0.8)
   0.5])

(defn setup []
  (q/color-mode :hsl 1.0)
  {:quadtree (spatialtree/quadtree 0 0 (q/width) (q/height))
   :boundary (rect/rect 0 0 (q/width) (q/height))
   :radius 1
   :circles []})

(defn contains-entity? [boundary {:keys [p r]}]
  (let [[x y] p]
    (and (> (- x r) (rect/left boundary))
         (< (+ x r) (rect/right boundary))
         (< (+ y r) (rect/top boundary))
         (> (- y r) (rect/bottom boundary)))))

(defn intersects [c1 c2]
  (when (geom/intersect-shape c1 c2) c2))

(defn add-circle [quadtree boundary search-radius radius]
  (let [r radius
        center (gv/vec2 (q/random r (- (q/width) r))
                        (q/random  r (- (q/height) r)))
        circle (assoc (gc/circle center r) :color (random-color))
        near (spatialtree/select-with-circle quadtree center search-radius)]
    (if (and (contains-entity? boundary circle)
             (not (some (partial intersects circle) near)))
      circle
      nil)))

(defn spatial-replace [tree {:keys [p] :as circle}]
  (geom/add-point (geom/delete-point tree p) p circle))

(defn grow [quadtree boundary search-radius circle]
  (if-not (:done circle)
    (let [growth (assoc (geom/scale-size circle 1.02) :color (:color circle))
          near (remove #{circle} (spatialtree/select-with-circle quadtree (:p growth) search-radius))]
      (if (and (contains-entity? boundary growth)
               (not (some (partial intersects growth) near)))
        growth
        (assoc circle :done true)))
    circle))

(defn max-radius [circles]
  (* 2 (apply max (map :r circles))))

(defn update-state [{:keys [boundary quadtree radius circles] :as state}]
  (let [search-radius (max-radius circles)
        circles' (map (partial grow quadtree boundary search-radius) circles)
        quadtree' (reduce spatial-replace quadtree (remove :done circles'))
        s' (assoc state :circles circles' :quadtree quadtree')]
    (if-let [circle (add-circle quadtree' boundary (max-radius circles') radius)]
      (-> s'
          (update :circles conj circle)
          (update :quadtree geom/add-point (:p circle) circle))
      s')))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.5)
  (doseq [{:keys [p r color done] :as circle} circles]
    (apply q/fill color)
    (if done
      (q/stroke 0 0.5 0.5 0.5)
      (q/stroke 0 0 0 1.0))
    (q/ellipse (:x p) (:y p) r r)))

(defn ^:export run-sketch []
  (q/defsketch circle-packing
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
