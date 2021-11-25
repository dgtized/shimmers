(ns shimmers.sketches.intertwined
  (:require
   [clojure.set :as set]
   [loom.alg :as la]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]))

;; Random path through a space, then subdivide into polygons where the path crosses itself
;; TODO: find polygons
;; Also is it useful/interesting to augment path to include each intersection point?
;; Adding dashes or varying the segment width?

(def modes [:intersections :graph])
(def edge-modes [:weighted-by-order :even-weight :hidden])
(def graph-modes [:faces :polygons])
(defonce ui-state (ctrl/state {:mode :intersections
                               :rows 3
                               :columns 4
                               :edge-mode :weighted-by-order
                               :graph-mode :faces}))
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

(defn collapse
  "Collapse consecutive points on the path, unioning associated segments together."
  [coll]
  (cs/collapse (fn [{a :p} {b :p}] (identical? a b))
               path-merge
               coll))

;; TODO: Bentley-Ottman line sweep for crossing line segments
;; https://en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
;; https://cs.stackexchange.com/questions/2717/polygons-generated-by-a-set-of-segments

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

;; FIXME: sometimes leader/trailing node has one connection despite being on an existing line segment?
;; This is not strictly necessary for polygon detection
(defn remove-tails
  "Remove any single connection tails from the intersection edge graph."
  [g]
  (loop [g g]
    (let [tails (filter (fn [v] (< (lg/out-degree g v) 2)) (lg/nodes g))]
      (if (empty? tails)
        g
        (recur (reduce lg/remove-nodes g tails))))))

(comment
  (do (def mvp (map gv/vec2 [[20 0] [20 20] [0 10] [0 20] [10 0] [10 10] [20 10] [10 20]]))
      (def medges
        (let [isecs (intersections mvp)]
          (intersections->edges isecs)))
      (def morig (poly-detect/edges->graph medges))
      (def mg (remove-tails morig)))
  ;; Contains three cycles, 2 triangles and a 5-gon plus removal of first and last point as tails
  (la/all-pairs-shortest-paths mg)
  (la/connected-components mg)
  (la/greedy-coloring mg)
  (lg/successors mg (gv/vec2 0 10))
  (la/bf-traverse mg (gv/vec2 0 10) :f vector)
  (la/bf-span mg (gv/vec2 0 10))
  (la/dijkstra-span mg (gv/vec2 0 10))
  (lg/weighted-digraph mg)
  (poly-detect/cycle-clockwise-from-edge mg (gv/vec2 0 10) (gv/vec2 4 12))
  (poly-detect/cycle-clockwise-from-edge mg (gv/vec2 4 12) (gv/vec2 0 10))
  (poly-detect/cycle-clockwise-from-edge mg (gv/vec2 4 12) (gv/vec2 10 0))
  (poly-detect/cycle-clockwise-from-edge mg (gv/vec2 10 0) (gv/vec2 4 12))
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
        {:keys [rows columns]} @ui-state
        zones (g/subdivide b {:rows rows :cols columns})
        k (* 0.1 (count zones))
        path (map g/centroid (drop k (dr/shuffle zones)))]
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
  (let [edge-mode (get-in @ui-state [:edge-mode])]
    (q/stroke 0)
    (case edge-mode
      :weighted-by-order (q/stroke 0)
      :even-weight (q/stroke-weight 0.5)
      :hidden (q/no-stroke))
    (let [segments (partition 2 1 path)
          segs (count segments)]
      (doseq [[idx [p q]] (map-indexed vector segments)]
        (when (= edge-mode :weighted-by-order)
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
        (q/pop-style)))

    (swap! defo assoc :intersections
           (->> intersects
                (filter (fn [isec] (near-mouse mouse (:p isec))))
                (map (fn [i] (update i :segments (partial mapv :points))))))))

(defn draw-graph [path mouse]
  (q/fill 0)
  (let [intersects (intersections path)
        edges (intersections->edges intersects)
        original (poly-detect/edges->graph edges)
        graph (remove-tails original)]
    (q/stroke-weight 0.5)
    (doseq [{:keys [p]} intersects]
      (cq/circle p 3.0))
    (swap! defo assoc :edges (filter (fn [[a b]] (near-mouse mouse a b)) edges))
    (doseq [[p q] edges
            :let [mouse-hit (near-mouse mouse p q)]]
      (q/stroke-weight (if mouse-hit 3.0 0.5))
      (q/line p q))
    (q/stroke-weight 0.5)

    (q/fill 0.0 1.0 1.0)
    (doseq [p (set/difference (lg/nodes original) (lg/nodes graph))]
      (cq/circle p 4.0))

    (q/fill 0.5 0.2)
    (let [graph-mode (get-in @ui-state [:graph-mode])]
      (if (= graph-mode :faces)
        (let [[p q] (poly-detect/edge-face-near-point graph mouse)
              cycle (poly-detect/polygon-near-point graph mouse)]
          (q/stroke 0.6 0.5 0.5 1.0)
          (q/stroke-weight 1.0)
          (cq/draw-shape cycle)
          (q/stroke-weight 2.5)
          (q/line p q)
          (q/no-stroke)
          (doseq [[i c] (map-indexed vector cycle)]
            (q/fill (/ i (count cycle)) 0.75 0.5)
            (cq/circle c 3.0))
          (swap! defo assoc
                 :cycle cycle
                 :edge-face [p q]))
        (let [polygons (poly-detect/simple-polygons graph)]
          (q/stroke 0 0.5)
          (q/stroke-weight 0.5)
          (swap! defo assoc :polygons polygons)
          (doseq [shape polygons]
            (-> shape
                (gp/inset-polygon -5.0)
                cq/draw-shape)))))))

(defn draw [{:keys [path mouse]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke 0)
  (reset! defo {})
  (case (:mode @ui-state)
    :intersections (draw-intersections path mouse)
    :graph (draw-graph path mouse)))

(defn ui-controls []
  (let [{:keys [mode]} @ui-state]
    [:div.flexcols {:style {:justify-content :space-between}}
     (ctrl/container ;; TODO: parameters?
      (ctrl/numeric ui-state "Rows" [:rows] [2 5 1])
      (ctrl/numeric ui-state "Columns" [:columns] [2 5 1])
      (ctrl/change-mode ui-state modes)
      (if (= mode :intersections)
        (ctrl/change-mode ui-state edge-modes :edge-mode)
        (ctrl/change-mode ui-state graph-modes :graph-mode)))
     [:div (view-sketch/generate :intertwined)]]))

(sketch/defquil intertwined
  :created-at "2021-10-23"
  :tags #{:deterministic}
  :on-mount (fn []
              (ctrl/mount ui-controls)
              (debug/mount defo))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
