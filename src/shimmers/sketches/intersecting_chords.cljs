(ns shimmers.sketches.intersecting-chords
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-circle [{p :p [w h] :size}]
  (let [r (dr/gaussian (/ h 12) (/ h 20))]
    (gc/circle (tm/+ p (gv/vec2 (dr/random r (- w r))
                                (dr/random r (- h r))))
               r)))

(defn legal-candidate
  [circletree
   {:keys [bounds gen-circle min-spacing max-spacing]}]
  (let [candidate (gen-circle)]
    (when (geometry/contains-circle? bounds candidate)
      (if-let [near (saq/closest-circle circletree candidate)]
        (let [d (saq/circle-overlap near candidate)]
          (when (and (not (collide/bounded? near candidate))
                     (< min-spacing d max-spacing))
            candidate))
        candidate))))

(defn pack-candidates
  [circletree legal-candidate n]
  (loop [i 0 tree circletree]
    (if (>= i n)
      tree
      (if-let [circle (legal-candidate tree)]
        (recur (inc i) (saq/add-point tree (:p circle) circle))
        (recur i tree)))))

(defn circle-pack [{[w h] :size :as bounds} n]
  (pack-candidates (saq/circletree bounds)
                   (fn [tree]
                     (legal-candidate
                      tree
                      {:bounds (g/scale-size bounds 0.95)
                       :gen-circle (partial make-circle bounds)
                       :min-spacing (- (* 0.1 (min w h)))
                       :max-spacing (* 0.05 (min w h))}))
                   n))

(defn rebuild-tree [bounds circles]
  (reduce (fn [t c] (saq/add-point t (:p c) c))
          (saq/circletree bounds)
          circles))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :circletree (circle-pack bounds 20)}))

(defn move [bounds circle]
  (or (when-let [v (:v circle)]
        (let [circle' (update circle :p tm/+ v)]
          (when (geometry/contains-circle? bounds circle')
            circle')))
      (assoc circle :v (dr/randvec2 1))))

(defn update-state [{:keys [bounds circletree] :as state}]
  (let [circles (->> circletree
                     saq/all-data
                     (map (partial move (g/scale-size bounds 0.95))))]
    (assoc state :circletree (rebuild-tree bounds circles))))

(defn draw [{:keys [circletree]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (doseq [{p :p :as circle} (saq/all-data circletree)]
    (q/stroke-weight 0.66)
    (q/stroke 0.66)
    (cq/circle circle)
    (q/stroke-weight 1.0)
    (doseq [nearby (saq/k-nearest-neighbors circletree 3 p)]
      (when-let [neighbor (g/get-point-data nearby)]
        (if (collide/overlaps? circle neighbor)
          (when-let [isecs (isec/intersect-circle-circle? circle neighbor)]
            (q/stroke 0.0)
            (apply q/line isecs))
          (do (q/stroke 0.0 0.5 0.25)
              (q/line (:p circle) (:p neighbor))))))))

(sketch/defquil intersecting-chords
  :created-at "2023-01-04"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
