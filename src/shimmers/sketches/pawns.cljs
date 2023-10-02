(ns shimmers.sketches.pawns
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]))

(defn make-grid [cols rows]
  (into {:dims [cols rows]}
        (for [i (range cols)
              j (range rows)]
          [[i j] nil])))

(defn neighbors [grid [x y]]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not (and (zero? i) (zero? j)))
        :let [pos [(+ x i) (+ y j)]]
        :when (not= :missing (get grid pos :missing))]
    pos))

(defn assign-target [state pos]
  (assoc-in state [:grid pos :target] {:items 0}))

(defn assign-block [state pos]
  (assoc-in state [:grid pos :block] {:items 1}))

(defn make-agent [pos id]
  {:pos pos :id id})

(defn move [{:keys [agents] :as state} id dest]
  (let [pos (get-in agents [id :pos])]
    (-> state
        (assoc-in [:agents id :pos] dest)
        (update-in [:grid pos] dissoc :agent)
        (assoc-in [:grid pos :agent] id))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [[cols rows] [30 20]
        agents [(make-agent [15 10] 0)]]
    {:agents agents
     :grid (as-> (make-grid cols rows) grid
             (reduce (fn [g {:keys [id pos]}] (assoc-in g [pos :agent] id))
                     grid agents)
             (reduce (fn [g pos]
                       (assoc-in g [pos :target] {:items 0}))
                     grid (for [k (range 6 16 2)] [1 k]))
             (reduce (fn [g pos]
                       (assoc-in g [pos :block] {:items 1}))
                     grid (repeatedly 64 (fn [] [(dr/random-int ( * 3 (/ cols 4)) (dec cols))
                                                (dr/random-int 1 (dec rows))]))))}))

(defn update-state [state]
  state)

(defn draw [{:keys [grid]}]
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
                (q/fill 0.0)
                (cq/circle (+ x (/ cell-w 2)) (+ y (/ cell-h 2)) (* cell-w 0.4))
                (q/no-fill))
              (> (get-in cell [:block :items] 0) 0)
              (do
                (q/fill 0.0)
                (q/rect (+ x (* 0.1 cell-w)) (+ y (* 0.1 cell-h))
                        (* 0.8 cell-w) (* 0.8 cell-h))
                (q/no-fill))
              (>= (get-in cell [:target :items] -1) 0)
              (do
                (q/stroke-weight 1.5)
                (q/rect (+ x (* 0.1 cell-w)) (+ y (* 0.1 cell-h))
                        (* 0.8 cell-w) (* 0.8 cell-h))
                (q/stroke-weight 1.0)))))))

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
