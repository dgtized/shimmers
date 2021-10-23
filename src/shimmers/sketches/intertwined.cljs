(ns shimmers.sketches.intertwined
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

;; Random path through a space, then subdivide into polygons where the path crosses itself
;; TODO: find polygons
;; Also is it useful/interesting to augment path to include each intersection point?
;; Adding dashes or varying the segment width?


(defn path-point [p segments joint]
  {:p p :segments (set segments) :joint joint})

(defn path-merge [prior curr]
  (assoc prior
         :segments (set/union (:segments prior) (:segments curr))
         :joint (or (:joint prior) (:joint curr))))

(defn intersect-point
  "Return point of intersection between two lines or nil."
  [l1 l2]
  (when-let [{:keys [type] :as hit} (g/intersect-line l1 l2)]
    (when (= type :intersect)
      (path-point (:p hit) [l1 l2] false))))

(defn collapse
  "Collapse consecutive points on the path, unioning associated segments together."
  [path]
  (loop [path path result [] prior (first path)]
    (if (empty? path)
      (conj result prior)
      (let [[curr & remains] path]
        (if (tm/delta= (:p prior) (:p curr))
          (recur remains result (path-merge prior curr))
          (recur remains (conj result prior) curr))))))

;; Might need path simplification, ie if a,b,c are all collinear just need a-c
;; However, path can double back onitself so requires some extra care
(defn intersections [path]
  (let [segments (map gl/line2 (partition 2 1 path))]
    (loop [intersections [(path-point (first path) (take 1 segments) true)]
           segments segments]
      (if (empty? segments)
        (collapse intersections)
        (let [[current & xs] segments
              hits (keep (partial intersect-point current) (rest xs))
              {[a b] :points} current
              ;; order points as distance along path
              ordered-hits (sort-by (fn [{:keys [p]}] (g/dist p a)) hits)
              joint (path-point b [current] true)]
          (recur (into intersections (conj ordered-hits joint)) xs))))))

(defn debug-isecs [path]
  (for [{:keys [p joint segments]} (intersections path)]
    {:p p
     :j joint
     :conns (disj (set (apply set/union (map :points segments))) p)}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [b (cq/screen-rect 0.99)
        zones (g/subdivide b {:rows 4 :cols 4})
        k (* 0.1 (count zones))
        path (map g/centroid (cons (first zones) (drop k (shuffle (rest zones)))))]
    #_(println (debug-isecs path))
    {:path path}))

(defn update-state [state]
  state)

(defn draw [{:keys [path]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (cq/draw-path path)
  (let [intersects (intersections path)
        isecs (count intersects)]
    (doseq [[idx {:keys [p joint]}] (map-indexed vector intersects)]
      (if joint
        (do (q/fill 0)
            (cq/circle p 2))
        (do (q/fill (/ idx isecs) 0.75 0.6)
            (cq/circle p (+ 3 (* 9 (- 1.0 (/ idx isecs))))))))))

(sketch/defquil intertwined
  :created-at "2021-10-23"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
