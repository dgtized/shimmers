(ns shimmers.sketches.pawns
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.quil :as cq]))

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

(defn make-agent [pos id]
  {:pos pos :id id})

(defn set-grid-position [grid {:keys [pos id]}]
  (assoc-in grid [pos :agent] id))

(defn move [{:keys [agents] :as state} id dest]
  (let [pos (get-in agents [id :pos])]
    (-> state
        (assoc-in [:agents id :pos] dest)
        (update-in [:grid pos] dissoc :agent)
        (assoc-in [:grid pos :agent] id))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [agents [(make-agent [15 10] 0)]]
    {:agents agents
     :grid (reduce (fn [g agent] (set-grid-position g agent))
                   (make-grid 30 20)
                   agents)}))

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
        (when (:agent cell)
          (q/fill 0.0)
          (cq/circle (+ x (/ cell-w 2)) (+ y (/ cell-h 2)) (* cell-w 0.4))
          (q/no-fill))))))

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
