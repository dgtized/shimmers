(ns shimmers.sketches.circle-packing
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 1)
  {:quadtree (spatialtree/quadtree 0 0 (q/width) (q/height))
   :boundary (rect/rect 0 0 (q/width) (q/height))
   :circles []})

(defn contains-entity? [boundary {:keys [p r]}]
  (let [[x y] p]
    (and (> (- x r) (rect/left boundary))
         (< (+ x r) (rect/right boundary))
         (< (+ y r) (rect/top boundary))
         (> (- y r) (rect/bottom boundary)))))

(defn intersects [c1 c2]
  (when (geom/intersect-shape c1 c2) c2))

(defn add-circle [quadtree boundary]
  (let [r 100
        center (gv/vec2 (q/random r (- (q/width) r))
                        (q/random  r (- (q/height) r)))
        circle (gc/circle center r)
        near (spatialtree/select-with-circle quadtree center (* r 10))]
    (if (and (contains-entity? boundary circle)
             (not (some (partial intersects circle) near)))
      circle
      nil)))

(defn spatial-replace [tree {:keys [p] :as circle}]
  (geom/add-point (geom/delete-point tree p) p circle))

(defn grow [quadtree boundary circle]
  (if-not (:done circle)
    (let [growth (geom/scale-size circle 1.1)
          near (spatialtree/select-with-circle quadtree (:p growth) (* (:r growth) 10))]
      (if (and (contains-entity? boundary growth)
               (not (some (partial intersects growth) near)))
        growth
        (do (println {:growth near :near near})
            (pr (some (partial intersects growth) near))
            (assoc circle :done true))))
    circle))

(defn update-state [{:keys [boundary quadtree circles] :as state}]
  (let [circles' (map (partial grow quadtree boundary) circles)
        quadtree' (reduce spatial-replace quadtree (remove :done circles'))
        s' (assoc state :circles circles' :quadtree quadtree')]
    (if-let [circle (add-circle quadtree' boundary)]
      (-> s'
          (update :circles conj circle)
          (update :quadtree geom/add-point (:p circle) circle))
      s')))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (doseq [{:keys [p r done]} circles]
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
