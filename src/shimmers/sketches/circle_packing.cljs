(ns shimmers.sketches.circle-packing
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.core :as sm]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; TODO: represent circles as polygons and grow individual points until it
;; intersects a neighbor and only mark the individual vertex as "done".
;; Scott says this should be called "cell-munging"

(defn random-color []
  [(rand)
   (q/random 0.15 0.9)
   (q/random 0.25 0.8)
   (q/random 0.25 0.8)])

(defn mixv [[c1 & v1] [c2 & v2] t]
  (into [(-> (sm/mix-mod c1 c2 t)
             (+ (* (q/random-gaussian) 0.05))
             (mod 1.0))]
        (mapv (fn [a b]
                (-> (tm/mix* a b t)
                    (+ (* (q/random-gaussian) 0.05))
                    (tm/clamp 0 1)))
              v1 v2)))

(defn color-mix [c1 c2]
  (if c2
    (let [[r1 r2] [(:r c1) (:r c2)]
          ;; proportional radius, ie if equal size use 0.5, otherwise weight
          ;; interpolation by color of larger radius
          t (/ r2 (+ r1 r2))
          mixed (mixv (:color c1) (:color c2) t)]
      mixed)
    (:color c1)))

(defn setup []
  (q/frame-rate 20)
  (q/color-mode :hsl 1.0)
  {:quadtree (spatialtree/quadtree 0 0 (q/width) (q/height))
   :boundary (rect/rect 0 0 (q/width) (q/height))
   :radius 2
   :scale 1.05
   :circles []})

(defn contains-entity? [boundary {:keys [p r]}]
  (let [[x y] p]
    (and (> (- x r) (rect/left boundary))
         (< (+ x r) (rect/right boundary))
         (< (+ y r) (rect/top boundary))
         (> (- y r) (rect/bottom boundary)))))

(defn intersects [c1 c2]
  ;; inside or intersecting from growth
  ;; TODO: add checkbox for allowing containment or not
  (when (geometry/circles-overlap? c1 c2)
    c2))

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

(defn grow [quadtree boundary search-radius scale circle]
  (if-not (:done circle)
    (let [growth (assoc (geom/scale-size circle scale) :color (:color circle))
          near (remove #{circle} (spatialtree/select-with-circle quadtree (:p growth) search-radius))
          intersecting-circle (some (partial intersects growth) near)]
      (if (and (contains-entity? boundary growth) (not intersecting-circle))
        growth
        (assoc circle :done true
               :color (color-mix circle intersecting-circle))))
    circle))

(defn max-radius [circles]
  (* 2 (apply max (map :r circles))))

(defn grow-circles [{:keys [boundary quadtree circles scale] :as state}]
  (let [search-radius (max-radius circles)
        circles' (map (partial grow quadtree boundary search-radius scale) circles)]
    (assoc state
           :circles circles'
           :quadtree (reduce spatial-replace quadtree (remove :done circles')))))

;; TODO performance from sampling cost?
(defn fresh-circles [state n]
  (loop [i 0 {:keys [boundary quadtree radius circles] :as state} state]
    (if (>= i n)
      state
      (if-let [circle (add-circle quadtree boundary (max-radius circles) radius)]
        (recur (inc i)
               (-> state
                   (update :circles conj circle)
                   (update :quadtree geom/add-point (:p circle) circle)))
        (recur i state)))))

;; Re-enable growth of nearby circles after cull?
(defn cull-circles [{:keys [circles quadtree] :as state} p]
  (let [culled (filter #(< (rand) p) circles)]
    (assoc state :circles (remove (set culled) circles)
           :quadtree (reduce geom/delete-point quadtree (map :p culled)))))

(defn update-state [state]
  (let [c (count (:circles state))
        pct (max 0 (- 1.0 (/ c 1600)))]
    (-> state
        grow-circles
        (fresh-circles (q/round (* (mod (+ c (q/frame-count)) 4) (rand) pct)))
        (cull-circles 0.00005))))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.5)
  (doseq [{:keys [p r color done]} circles]
    (apply q/fill color)
    (if done
      (q/no-stroke)
      (q/stroke 0 0 0 1.0))
    (cq/circle p r)))

(sketch/defquil circle-packing
  :created-at "2021-03-10"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
