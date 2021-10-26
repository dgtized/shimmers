(ns shimmers.sketches.rtree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.rtree :as rtree]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:shapes 1000
               :lower 3.0
               :upper 3.0
               :max-children 10}))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [shapes lower upper]} @ui-state
        random-circle
        (fn [] (gc/circle (map int (cq/rel-vec (rand) (rand)))
                         (int (tm/random lower upper))))]
    {:circles (repeatedly shapes random-circle)}))

(defn update-state [{:keys [circles] :as state}]
  (let [mp (cq/mouse-position)
        tree (rtree/create (select-keys @ui-state [:max-children]) circles)
        path (rtree/path-search tree mp)]
    (reset! defo {:mouse mp
                  :hit (:data (last path))
                  :bounds (map :bounds path)})
    (assoc state
           :rtree tree
           :path path)))

(defn tree-walk-with-depth [rtree]
  (tree-seq (fn [x] (not-empty (:children x)))
            (fn [{:keys [depth children]}]
              (map #(assoc % :depth (inc depth)) children))
            (assoc rtree :depth 0)))

(defn draw [{:keys [rtree path]}]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke-weight 1.0)
  (let [tree (tree-walk-with-depth rtree)
        max-depth (apply max (map :depth tree))]
    (doseq [{:keys [bounds data depth]} tree]
      (q/stroke (/ depth (inc max-depth)))
      (cq/rectangle bounds)
      (when-let [{p :p r :r} data]
        (q/stroke 0.0 0.6 0.6 1.0)
        (cq/circle p r)))

    (q/stroke-weight 1.5)
    (q/stroke 0.6 0.4 0.5)
    (doseq [{:keys [bounds]} path]
      (cq/rectangle bounds))))

(defn ui-controls []
  (let [{:keys [lower upper]} @ui-state]
    [:div
     [:h4 "Requires Restart"]
     [:div (ctrl/slider ui-state (fn [v] (str "Seed Shapes " v)) [:shapes] [100 2000 100])]
     [:p [:em "Radius Bounds"]]
     [:div
      (ctrl/numeric ui-state "Lower" [:lower] [1.0 (min upper 16) 1.0])
      (ctrl/numeric ui-state "Upper" [:upper] [(max 1.0 lower) 16 1.0])]
     [:h4 "On Demand"]
     [:div (ctrl/slider ui-state (fn [v] (str "Max Children " v)) [:max-children] [2 32 1])]
     ;; Debug output on hit path and mouse location
     (debug/display defo)]))

(sketch/defquil rtree
  :created-at "2021-10-09"
  :tags #{:datastructures}
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
