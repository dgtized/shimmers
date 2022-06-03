(ns shimmers.sketches.the-journey-between
  (:require
   [loom.alg :as la]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.sketch]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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
    (assoc loc :noise (Math/pow noise 1.25))))

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
     (let [h1 (:noise (loc-grid grid-size grid n1))
           h2 (:noise (loc-grid grid-size grid n2))]
       (+ 1 (if (> h2 h1)
              (* 2.0 (- h2 h1)) ;; up-hill
              (- 0.33 (- h1 h2)) ;; down-hill
              ))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [seed (gv/vec2 (dr/random 100) (dr/random 100))
        grid-size {:cols 40 :rows 30}
        grid (build-grid seed grid-size)]
    ;; prevent right click contextmenu on the sketch canvas so can capture right
    ;; mouse button location without a menu
    (.addEventListener (.-canvas (quil.sketch/current-applet))
                       "contextmenu" (fn [e] (.preventDefault e)))
    {:seed seed
     :grid-size grid-size
     :grid grid
     :mouse {:right (to-grid-loc grid-size (gv/vec2))
             :left (to-grid-loc grid-size (tm/+ (cq/rel-vec 0.6 0.6) (dr/jitter (cq/rel-h 0.1))))}
     :path {}
     :graph (make-flygraph grid-size grid (gv/vec2))}))

(defn mouse-button-clicked
  [mouse grid-size]
  (cond (q/mouse-pressed?)
        (let [pos (cq/mouse-position)
              button (q/mouse-button)]
          (if (and (g/contains-point? (cq/screen-rect) pos)
                   (#{:left :right} button))
            (assoc mouse button (to-grid-loc grid-size pos))
            mouse))
        :else
        mouse))

(defn update-state [{:keys [mouse graph grid-size path] :as state}]
  (let [{src :right dst :left :as mouse'}
        (mouse-button-clicked mouse grid-size)]
    (if (or (not= mouse mouse') (empty? path))
      (assoc state
             :mouse mouse'
             :path (la/astar-path graph src dst (partial g/dist dst)))
      state)))

(defn backtrack [current path]
  (cons current
        (lazy-seq (when-let [parent (get path current)]
                    (backtrack parent path)))))

(defn draw [{:keys [mouse grid grid-size path]}]
  (let [{src :right dst :left} mouse]
    (reset! defo {:source [src :-> (loc-grid grid-size grid src)]
                  :dest [dst :-> (loc-grid grid-size grid dst)]
                  :path (count path)})
    (q/no-stroke)
    (doseq [{[x y] :p [w h] :size noise :noise} grid]
      (q/fill 0.0 0.0 noise 1.0)
      (q/rect x y w h))

    (q/stroke-weight 3.0)
    (doseq [[i q] (map-indexed vector (backtrack dst path))]
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
