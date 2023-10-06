(ns shimmers.sketches.pawns
  (:require
   [loom.alg :as la]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-grid [cols rows]
  (into {:dims [cols rows]}
        (for [i (range cols)
              j (range rows)]
          [[i j] nil])))


(defn make-agent [pos id]
  {:pos pos :id id :mode :start})

(defn add-blocks [grid cols rows]
  (reduce (fn [g pos]
            (update-in g [pos :block :items] (fnil inc 0)))
          grid (repeatedly 64 (fn [] [(dr/random-int ( * 3 (/ cols 4)) (dec cols))
                                     (dr/random-int 1 (dec rows))]))))

(defn add-targets [grid targets]
  (reduce (fn [g pos]
            (assoc-in g [pos :target] {:items 0}))
          grid targets))

(defn init-state [cols rows]
  (let [agents [(make-agent [15 6] 0)
                (make-agent [15 10] 1)
                (make-agent [15 14] 2)]]
    {:agents agents
     :grid (as-> (make-grid cols rows) grid
             (reduce (fn [g {:keys [id pos]}] (assoc-in g [pos :agent] id))
                     grid agents)
             (add-targets grid (for [k (range 6 16 2)] [1 k]))
             (add-blocks grid cols rows))}))

(defn neighbors [grid [x y]]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not (and (zero? i) (zero? j)))
        :let [pos (gv/vec2 (+ x i) (+ y j))]
        :when (not= :missing (get grid pos :missing))]
    pos))

(defn neighbor? [[ax ay] [bx by]]
  (let [dx (Math/abs (- ax bx))
        dy (Math/abs (- ay by))]
    (or (and (= dx 1) (= dy 0))
        (and (= dx 0) (= dy 1))
        (and (= dx 1) (= dy 1)))))

(defn passable? [grid pos]
  (let [cell (get-in grid [pos])]
    (not (or (and (:block cell) (> (:items (:block cell)) 0))
             (:agent cell)))))

(comment
  (neighbors (make-grid 5 5) [2 2])
  (neighbors (make-grid 5 5) [0 0]))

(defn move [{:keys [agents] :as state} id dest]
  (let [pos (get-in agents [id :pos])]
    (-> state
        (assoc-in [:agents id :pos] dest)
        (update-in [:grid pos] dissoc :agent)
        (assoc-in [:grid dest :agent] id))))

(defn manhattan [from p]
  (apply + (tm/abs (tm/- (gv/vec2 p) (gv/vec2 from)))))

(defn find-closest [grid from element]
  (when-let [positions (seq (keep (fn [[pos value]]
                                    (when-let [cell (element value)]
                                      (case element
                                        :target
                                        (gv/vec2 pos)
                                        :block
                                        (when (and (not (:target value))
                                                   (> (get cell :items 0) 0))
                                          (gv/vec2 pos)))))
                                  (dissoc grid :dims)))]
    (apply min-key (fn [p] (manhattan from p)) positions)))

(defn backtrack [current path]
  (cons current
        (lazy-seq (when-let [parent (get path current)]
                    (backtrack parent path)))))

(defn search-path [grid start dest]
  (when-let [path (-> (lg/fly-graph :start start
                                    :successors (fn [loc] (neighbors grid loc))
                                    :weight (fn [_l1 _l2] 1))
                      (la/astar-path start dest g/dist))]
    (reverse (backtrack dest path))))

(comment
  (find-closest (:grid (init-state 5 5)) [1 1] :block)
  (find-closest (:grid (init-state 7 5)) [1 1] :target)
  (search-path (:grid (init-state 5 5)) [0 0] [4 4]))

(defn move-path
  [{:keys [grid] :as state}
   {:keys [id pos dest] :as agent}]
  (let [pos' (first (seq (:path agent)))]
    (if pos'
      (if (passable? grid pos')
        (-> state
            (move id pos')
            (update-in [:agents id :path] rest))
        (if-let [sidesteps (seq (keep (fn [p] (when (passable? grid p) p))
                                      (neighbors grid pos)))]
          (-> state
              (move id (dr/rand-nth sidesteps))
              (update-in [:agents id] dissoc :path))
          state))
      (assoc-in state [:agents id :path]
                (rest (butlast (search-path grid pos dest)))))))

(defn pickup-block
  [{:keys [grid] :as state}
   {:keys [id pos dest]}]
  (if (> (get-in state [:grid dest :block :items]) 0)
    (-> state
        (update-in [:grid dest :block :items] dec)
        (update-in [:agents id]
                   (fn [agent] (-> agent
                                  (assoc :mode :carrying)
                                  (update :items (fnil inc 0))
                                  (assoc :dest
                                         (find-closest grid pos :target))))))
    (update-in state [:agents id]
               (fn [agent] (-> agent
                              (assoc :mode :start)
                              (dissoc :dest))))))

(defn drop-block
  [state {:keys [id dest]}]
  (-> state
      (update-in [:grid dest :block :items] (fnil inc 0))
      (update-in [:agents id]
                 (fn [agent] (-> agent
                                (assoc :mode :start)
                                (update :items dec)
                                (dissoc :dest))))))

;; should only recompute search-path if next step is invalid
;; also need to update path better so it shows you can't go through blocks
(defn update-agent [{:keys [grid] :as state} id]
  (let [{:keys [pos dest mode] :as agent} (get-in state [:agents id])]
    (case mode
      :start
      (if-let [dest (find-closest grid pos :block)]
        (-> state
            (update-in [:agents id] assoc
                       :mode :moving
                       :dest dest))
        state)
      :moving
      (if (neighbor? pos dest)
        (pickup-block state agent)
        (move-path state agent))

      :carrying
      (if (neighbor? pos dest)
        (drop-block state agent)
        (move-path state agent)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  ;; (q/frame-rate 10)
  (init-state 30 20))

(defn update-state [{:keys [agents] :as state}]
  (reduce update-agent state (range (count agents))))

(defn draw [{:keys [grid agents]}]
  (q/background 1.0)
  (q/no-fill)
  (q/ellipse-mode :radius)
  (let [[cols rows] (:dims grid)
        cell-w (/ (q/width) cols)
        cell-h (/ (q/height) rows)]
    (doseq [i (range cols)
            j (range rows)]
      (let [cell (get grid [i j])
            x (* i cell-w)
            y (* j cell-h)]
        (q/rect x y cell-w cell-h)
        (cond (:agent cell)
              (do
                (q/fill 0.6 0.5 0.5 1.0)
                (cq/circle (+ x (/ cell-w 2)) (+ y (/ cell-h 2)) (* cell-w 0.4))
                (when (= (:mode (nth agents (:agent cell))) :carrying)
                  (q/fill 0.0)
                  (q/rect (+ x (* 0.25 cell-w)) (+ y (* 0.25 cell-h))
                          (* 0.5 cell-w) (* 0.5 cell-h)))
                (q/no-fill))
              (:target cell)
              (do
                (q/stroke-weight 1.5)
                (q/rect (+ x (* 0.1 cell-w)) (+ y (* 0.1 cell-h))
                        (* 0.8 cell-w) (* 0.8 cell-h))
                (let [items (get-in cell [:block :items] 0)]
                  (when (> items 0)
                    (q/fill 0.0)
                    (let [s (str items)
                          tw (q/text-width s)
                          th (+ (* 0.66 (q/text-ascent)) (q/text-descent))]
                      (q/text s
                              (+ x (/ (- cell-w tw) 2)) (+ y (/ (- cell-h th) 2))
                              (+ x cell-w) (+ y cell-h)))
                    (q/no-fill)))
                (q/stroke-weight 1.0))
              (> (get-in cell [:block :items] 0) 0)
              (do
                (q/fill 0.0)
                (q/rect (+ x (* 0.1 cell-w)) (+ y (* 0.1 cell-h))
                        (* 0.8 cell-w) (* 0.8 cell-h))
                (q/no-fill)))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition pawns
  {:created-at "2023-10-01"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
