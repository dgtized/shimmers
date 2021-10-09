(ns shimmers.sketches.rtree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.rtree :as rtree]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]))

(defonce ui-state
  (ctrl/state {:shapes 1000
               :max-children 10}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles (repeatedly (:shapes @ui-state) #(gc/circle (cq/rel-vec (rand) (rand)) 2.5))})

(defn update-state [{:keys [circles] :as state}]
  (assoc state :rtree (rtree/create (select-keys @ui-state [:max-children]) circles)))

(defn tree-walk-with-depth [rtree]
  (tree-seq (fn [x] (not-empty (:children x)))
            (fn [{:keys [depth children]}]
              (map #(assoc % :depth (inc depth)) children))
            (assoc rtree :depth 0)))

(defn draw [{:keys [rtree]}]
  (q/background 1.0)
  (q/no-fill)
  (let [tree (tree-walk-with-depth rtree)
        max-depth (apply max (map :depth tree))]
    (doseq [{:keys [bounds data depth]} tree]
      (q/stroke (/ depth (inc max-depth)))
      (cq/rectangle bounds)
      (when-let [{p :p r :r} data]
        (q/stroke 0.0 0.6 0.6 1.0)
        (cq/circle p r)))))

(defn ui-controls []
  [:div
   [:em "Requires Restart"]
   (ctrl/slider ui-state (fn [v] (str "Seed Shapes " v)) [:shapes] [100 2000 100])
   [:p]
   [:em "On Demand"]
   (ctrl/slider ui-state (fn [v] (str "Max Children " v)) [:max-children] [2 32 1])])

(sketch/defquil rtree
  :created-at "2021-10-09"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
