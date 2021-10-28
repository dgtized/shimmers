(ns shimmers.sketches.intertwined
  (:require
   [clojure.set :as set]
   [loom.alg :as la]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Random path through a space, then subdivide into polygons where the path crosses itself
;; TODO: find polygons
;; Also is it useful/interesting to augment path to include each intersection point?
;; Adding dashes or varying the segment width?

(def modes [:intersections :graph])
(defonce ui-state (ctrl/state {:mode :intersections
                               :edges {:weighted-by-order true}}))
(defonce defo (debug/state))

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

;; TODO: consider generalizing this for any consecutive sequence, parameterized
;; by same? and a merge function? Possibly handling chunkedseq?
(defn collapse
  "Collapse consecutive points on the path, unioning associated segments together.

  same? is parameterized as identical? to ease substitution with tm/delta= if
  point sets are not from the same dataset."
  [path & {:keys [same?] :or {same? identical?}}]
  (loop [path path prior (first path) result []]
    (if (empty? path)
      (conj result prior)
      (let [[curr & remains] path]
        (if (same? (:p prior) (:p curr))
          (recur remains (path-merge prior curr) result)
          (recur remains curr (conj result prior)))))))

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

(defn vertices-per-isec
  "Calculate the set of vertices for each segment from the intersections."
  [intersections]
  (->> (for [{:keys [p segments]} intersections
             seg segments]
         {seg #{p}})
       (apply (partial merge-with set/union))))

;; WIP just trying to get basic output here. I think problems with directed vs undirected graph?
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

(defn edges->graph [edges]
  (reduce (fn [g [a b]] (lg/add-edges g [a b (g/dist a b)]))
          (lg/weighted-graph) edges))

;; FIXME: sometimes leader/trailing node has one connection despite being on an existing line segment?
(defn remove-tails
  "Remove any single connection tails from the intersection edge graph."
  [g]
  (loop [g g]
    (let [tails (filter (fn [v] (< (lg/out-degree g v) 2)) (lg/nodes g))]
      (if (empty? tails)
        g
        (recur (reduce lg/remove-nodes g tails))))))

(defn cycle-clockwise [g start]
  (loop [cycle [] vertex start]
    (cond (and (seq cycle) (identical? vertex start))
          cycle
          (and (seq cycle) (some (partial identical? vertex) cycle))
          []
          :else
          (let [candidates (lg/successors g vertex)
                candidates (if (empty? cycle) candidates
                               (remove (partial identical? (last cycle)) candidates))]
            (if (empty? candidates)
              []
              (recur (conj cycle vertex)
                     (apply max-key (fn [p] (g/heading (tm/- p vertex)))
                            candidates)))))))

(comment
  (do (def mvp (map gv/vec2 [[20 0] [20 20] [0 10] [0 20] [10 0] [10 10] [20 10] [10 20]]))
      (def medges
        (let [isecs (intersections mvp)]
          (intersections->edges isecs)))
      (def morig (edges->graph medges))
      (def mg (remove-tails morig)))
  ;; Contains three cycles, 2 triangles and a 5-gon plus removal of first and last point as tails
  (la/all-pairs-shortest-paths mg)
  (la/connected-components mg)
  (la/greedy-coloring mg)
  (lg/successors mg [0 10])
  (let [prev (gv/vec2 0 10)
        n (gv/vec2 4 12)
        neighbors (lg/successors mg n)
        angle (fn [p] (g/heading (tm/- p n)))]
    [neighbors (g/heading (tm/- n prev)) (map angle neighbors)
     (apply max-key (fn [p] (g/heading (tm/- p n)))
            neighbors)])
  (cycle-clockwise mg (gv/vec2 0 10))
  (cycle-clockwise mg (gv/vec2 4 12))
  (cycle-clockwise mg (gv/vec2 20 20))
  )

(defn debug-isecs [state path]
  (assoc state
         :path
         (for [{:keys [p joint segments]} (intersections path)]
           [(if joint :j :p) p
            :c (disj (set (apply set/union (map :points segments))) p)])))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [b (cq/screen-rect 0.99)
        zones (g/subdivide b {:rows 4 :cols 4})
        k (* 0.1 (count zones))
        path (map g/centroid (drop k (shuffle zones)))]
    #_(swap! defo debug-isecs path)
    {:mouse (gv/vec2)
     :path path}))

(defn update-state [state]
  (assoc state :mouse (cq/mouse-position)))

(defn near-mouse
  ([m p]
   (< (g/dist-squared m p) 32))
  ([m p q]
   (or (near-mouse m p) (near-mouse m q))))

(defn draw-segments [path]
  (let [weighted-by-order (get-in @ui-state [:edges :weighted-by-order])]
    (when-not weighted-by-order
      (q/stroke-weight 0.5))
    (let [segments (partition 2 1 path)
          segs (count segments)]
      (doseq [[idx [p q]] (map-indexed vector segments)]
        (when weighted-by-order
          (q/stroke-weight (+ 0.5 (* 1.0 (/ idx segs)))))
        (q/line p q)))))

(defn draw-intersections [path mouse]
  (draw-segments path)
  (q/stroke-weight 0.5)

  (let [intersects (intersections path)
        isecs (count intersects)]
    (doseq [[idx {:keys [p segments joint]}] (map-indexed vector intersects)]
      (q/fill (/ idx isecs) 0.75 0.6)
      (cq/circle p (+ 3 (* 9 (- 1.0 (/ idx isecs)))))
      (when joint
        (q/fill 0)
        (cq/circle p 2))
      (when (near-mouse mouse p)
        (q/push-style)
        (q/stroke-weight 3.0)
        (doseq [{[a b] :points} segments]
          (q/line a b))
        (q/pop-style)))))

(defn draw-graph [path mouse]
  (q/fill 0)
  (let [intersects (intersections path)
        edges (intersections->edges intersects)
        original (edges->graph edges)
        graph (remove-tails original)]
    (q/stroke 0)
    (q/stroke-weight 0.5)
    (doseq [{:keys [p]} intersects]
      (cq/circle p 3.0))
    (swap! defo assoc :edges (filter (fn [[a b]] (near-mouse mouse a b)) edges))
    (doseq [[p q] edges
            :let [mouse-hit (near-mouse mouse p q)]]
      (q/stroke-weight (if mouse-hit 3.0 0.5))
      (q/line p q))
    (q/fill 0.0 1.0 1.0)
    (doseq [p (set/difference (lg/nodes original) (lg/nodes graph))]
      (cq/circle p 4.0))

    (q/fill 0.9)
    (let [start (apply min-key (fn [p] (g/dist-squared mouse p)) (lg/nodes graph))
          cycle (cycle-clockwise graph start)]
      (cq/draw-shape cycle)
      (cq/circle start 3.0)
      (swap! defo assoc :cycle cycle))
    ))

(defn draw [{:keys [path mouse]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (reset! defo {})
  (case (:mode @ui-state)
    :intersections (draw-intersections path mouse)
    :graph (draw-graph path mouse)))

(defn ui-controls []
  (let [{:keys [mode]} @ui-state]
    [:div
     (ctrl/change-mode ui-state modes)
     (when (= mode :intersections)
       (ctrl/checkbox ui-state "Edges Weighted By Order" [:edges :weighted-by-order]))]))

(sketch/defquil intertwined
  :created-at "2021-10-23"
  :on-mount (fn []
              (ctrl/mount ui-controls)
              (debug/mount defo))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
