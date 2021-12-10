(ns shimmers.sketches.spaces-divided
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn gen-line [bounds]
  (fn []
    (let [[a b c d] (g/edges bounds)
          [[p1 q1] [p2 q2]] (dr/rand-nth [[a c] [b d]])]
      (gl/line2 (tm/mix p1 q1 (dr/random 0.1 0.9))
                (tm/mix p2 q2 (dr/random 0.1 0.9))))))

(defn isec-point [l1 l2]
  (when-let [{:keys [type] :as hit} (g/intersect-line l1 l2)]
    (when (= type :intersect)
      [(:p hit) #{l1 l2}])))

(defn line-intersections [lines]
  (remove nil?
          (for [[a b] (cs/all-pairs lines)]
            (isec-point a b))))

(defn vertices-per-isec
  "Calculate the set of vertices for each line from the intersections."
  [intersections]
  (->> (for [[p lines] intersections
             line lines]
         {line #{p}})
       (apply (partial merge-with set/union))))

(defn intersections->edges [isecs]
  (apply set/union
         (for [[{[a b] :points} vertices] (vertices-per-isec isecs)]
           (let [ordered (sort-by (fn [p] (g/dist a p)) (conj vertices a b))]
             (->> ordered
                  dedupe ;; sometimes a or b is already in points
                  (partition 2 1)
                  ;; ensure edges are always low pt -> high pt
                  (map (fn [v] (sort v)))
                  set)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :lines (repeatedly 6 (gen-line bounds))}))

(defn update-state [{:keys [lines] :as state}]
  (let [isecs (line-intersections lines)]
    (assoc state
           :intersections isecs
           :edges (intersections->edges isecs))))

(defn draw [{:keys [lines intersections edges]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (swap! defo assoc :isecs (map first intersections)
         :edges edges)

  (doseq [{[p q] :points} lines]
    (q/line p q))

  (doseq [p (map first intersections)]
    (cq/circle p 3.0))

  (q/stroke-weight 1.5)
  (doseq [[p q] edges]
    (q/line p q))

  (q/stroke-weight 0.5)
  (doseq [poly (-> edges
                   poly-detect/edges->graph
                   poly-detect/simple-polygons)]
    (cq/draw-shape (gp/inset-polygon poly -5.0))))

(sketch/defquil spaces-divided
  :created-at "2021-12-09"
  :size [800 600]
  :on-mount #(debug/mount defo)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
