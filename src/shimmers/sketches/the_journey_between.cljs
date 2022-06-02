(ns shimmers.sketches.the-journey-between
  (:require
   [loom.alg :as la]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defonce defo (debug/state))

(defn neighbors [[x y]]
  (->> [[(inc x) y]
        [(dec x) y]
        [x (inc y)]
        [x (dec y)]]
       (map gv/vec2)))

(defn in-bounds? [{:keys [rows cols]} [x y]]
  (and (<= 0 x) (< x cols) (<= 0 y) (< y rows)))

(defn neighborhood [grid-size loc]
  (filter (partial in-bounds? grid-size) (neighbors loc)))

(defn build-grid [seed grid-size]
  (for [loc (g/subdivide (cq/screen-rect) grid-size)
        :let [noise (dr/noise-at-point seed 0.01 (g/centroid loc))]]
    (assoc loc :noise (- 1.0 noise))))

(defn to-grid-loc [{:keys [rows cols]} [x y]]
  (gv/vec2 (int (* cols (/ x (q/width))))
           (int (* rows (/ y (q/height))))))

(defn loc-grid [{:keys [cols]} grid [i j]]
  (nth grid (+ (* j cols) i)))

(defn make-flygraph [grid-size grid start]
  (lg/fly-graph
   :start start
   :successors (fn [loc] (neighborhood grid-size loc))
   :predecessors
   (fn [loc] (neighborhood grid-size loc))
   :weight
   (fn [n1 n2]
     (+ 2 (- (:noise (loc-grid grid-size grid n2))
             (:noise (loc-grid grid-size grid n1)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [seed (gv/vec2 (dr/random 100) (dr/random 100))
        grid-size {:cols 40 :rows 30}
        grid (build-grid seed grid-size)]
    {:seed seed
     :grid-size grid-size
     :grid grid
     :mouse (gv/vec2)
     :graph (make-flygraph grid-size grid (gv/vec2))}))

(defn update-state [state]
  (update state :mouse cq/mouse-last-position-clicked))

(defn backtrack [current path]
  (cons current
        (lazy-seq (when-let [parent (get path current)]
                    (backtrack parent path)))))

(defn draw [{:keys [mouse grid grid-size graph]}]
  (let [start (gv/vec2)
        dest (to-grid-loc grid-size mouse)
        path (la/astar-path graph start dest (partial g/dist dest))]
    (reset! defo {:destination dest
                  :grid-cell [(loc-grid grid-size grid dest)
                              (g/centroid (loc-grid grid-size grid dest))]
                  :path path
                  :p2 (map (fn [p] (loc-grid grid-size grid p)) (backtrack dest path))})
    (q/no-stroke)
    (doseq [{[x y] :p [w h] :size noise :noise} grid]
      (q/fill 0.0 0.0 noise 1.0)
      (q/rect x y w h))

    (q/stroke-weight 3.0)
    (doseq [[i q] (map-indexed vector (backtrack dest path))]
      (q/stroke (/ i (count path)) 0.5 0.5)
      (when-let [p (get path q)]
        (q/line (g/centroid (loc-grid grid-size grid p))
                (g/centroid (loc-grid grid-size grid q)))))))

(sketch/defquil the-journey-between
  :created-at "2022-06-02"
  :size [800 600]
  :on-mount #(debug/mount defo)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
