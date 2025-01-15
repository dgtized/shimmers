(ns shimmers.sketches.flipboard
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn gen-grid [square n]
  (let [side (/ (g/height square) n)]
    (into {}
          (for [i (range n)
                j (range n)
                :let [rx (/ (float i) n)
                      ry (/ (float j) n)]]
            [[i j] {:cell (g/scale-size (rect/rect (g/unmap-point square (gv/vec2 rx ry))
                                                   side side)
                                        0.95)
                    :value (dr/random)}]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n 64
        square (g/center (rect/rect (cq/rel-h 0.99)) (cq/rel-vec 0.5 0.5))]
    {:grid (gen-grid square n)}))

(defn neighbors [grid [i j]]
  (for [d [[-1 0] [1 0] [0 1] [0 -1]]
        :let [pos (map + [i j] d)]
        :when (get grid pos)]
    pos))

(comment (neighbors (gen-grid (rect/rect 10 10) 4) [0 0]))

(defn flood [grid criteria start depth]
  (loop [depth depth region #{start} frontier (neighbors grid start)]
    (if (or (zero? depth) (empty? frontier))
      region
      (let [accepted (filter criteria frontier)
            region' (into region accepted)]
        (recur (dec depth)
               region'
               (distinct (mapcat (fn [cell] (remove region' (neighbors grid cell))) accepted)))))))

(defn flip [value target delta]
  (tm/clamp01
   (if (< target 0.5)
     (+ value (dr/random delta))
     (- value (dr/random delta)))))

(comment
  (let [grid (gen-grid (rect/rect 10 10) 4)]
    (flood grid
           (fn [cell]
             (<= (abs (- (:value (get grid cell)) 1.0)) 0.25))
           [0 0]
           2)))

(defn update-grid [grid]
  (let [cell (dr/rand-nth (keys grid))
        target (:value (get grid cell))
        criteria (fn [cell] (<= (abs (- target (:value (get grid cell)))) 0.2))
        depth (dr/weighted {0 2 1 5 2 2 3 1 4 1 5 1 6 1 7 1 8 1
                            9 1 10 1 11 1 12 1 13 1 14 1 15 1 16 1})
        connected (flood grid criteria cell depth)
        delta (dr/random 0.1 0.25)]
    (reduce (fn [grid cell]
              (update-in grid [cell :value] flip target delta))
            grid
            connected)))

(defn update-state [state]
  (nth (iterate (fn [s] (update s :grid update-grid))
                state)
       (dr/random-int 6)))

(defn draw [{:keys [grid]}]
  (q/background 1.0)
  (q/no-stroke)
  (doseq [{:keys [cell value]} (vals grid)]
    (q/fill (if (< value 0.5) 0 1))
    (qdg/draw cell)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition flipboard
  {:created-at "2025-01-14"
   :tags #{:genuary2025}
   :type :quil}
  (ctrl/mount page))
