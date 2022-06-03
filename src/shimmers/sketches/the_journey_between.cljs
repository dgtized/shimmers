(ns shimmers.sketches.the-journey-between
  (:require
   [loom.alg :as la]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [quil.sketch]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
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

(defn edge-weight [grid-size grid]
  (let [lookup (fn [n] (:noise (loc-grid grid-size grid n)))]
    (fn [n1 n2]
      (let [h1 (int (* 100 (lookup n1)))
            h2 (int (* 100 (lookup n2)))]
        (+ 10 (if (> h2 h1)
                (* 50 (- h2 h1)) ;; up-hill
                (* 2 (- h1 h2)) ;; down-hill
                ))))))

(defn make-flygraph [grid-size grid start]
  (lg/fly-graph
   :start start
   :successors (fn [loc] (neighborhood grid-size loc))
   :predecessors
   (fn [loc] (neighborhood grid-size loc))
   :weight (edge-weight grid-size grid)))

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
             :path (la/astar-path graph src dst g/dist))
      state)))

(defn backtrack [current path]
  (cons current
        (lazy-seq (when-let [parent (get path current)]
                    (backtrack parent path)))))

(defn draw [{:keys [mouse grid grid-size path]}]
  (let [{src :right dst :left} mouse
        back-path (backtrack dst path)
        weight (edge-weight grid-size grid)
        costs (map (fn [[p q]] (weight p q)) (partition 2 1 back-path))]
    (reset! defo {:source [src (loc-grid grid-size grid src)]
                  :dest [dst (loc-grid grid-size grid dst)]
                  :cost [(count path)
                         (reduce + costs)]
                  :path (map vector back-path costs)})
    (q/no-stroke)
    (doseq [{[x y] :p [w h] :size noise :noise} grid]
      (q/fill 0.0 0.0 noise 1.0)
      (q/rect x y w h))

    (q/stroke-weight 3.0)
    (doseq [[i q] (map-indexed vector back-path)]
      (q/stroke (/ i (count path)) 0.5 0.5)
      (when-let [p (get path q)]
        (q/line (g/centroid (loc-grid grid-size grid p))
                (g/centroid (loc-grid grid-size grid q)))))))

(defn ui-controls []
  (let [{:keys [source dest cost path]} @defo]
    [:div.flexcols
     [:div {:style {:width "15em"}}
      [:div [:h5 "Source"] (debug/pre-edn source {:width 40})]
      [:div [:h5 "Destination"] (debug/pre-edn dest {:width 40})]
      [:div [:h5 "Cost"] (debug/pre-edn cost)]]
     [:div
      [:div {:style {:font-size "0.75em"}}
       [:h5 "Path"]
       (debug/pre-edn path)]]]))

(sketch/defquil the-journey-between
  :created-at "2022-06-02"
  :size [800 600]
  :on-mount #(ctrl/mount ui-controls)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
