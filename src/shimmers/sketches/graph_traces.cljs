(ns shimmers.sketches.graph-traces
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]))

(defn new-graph []
  (let [nodes (zipmap (map (comp keyword char) (range 65 (+ 65 26)))
                      (repeatedly 11 #(rp/sample-point-inside (cq/screen-rect 0.85))))]
    {:nodes nodes
     :edges (for [[p q] (->> nodes
                             keys
                             cs/all-pairs
                             (sort-by (fn [[p q]] (g/dist (get nodes p) (get nodes q))))
                             (take 33))]
              {:p p :q q})}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:graph (new-graph)})

(defn update-state [state]
  state)

(defn draw [{:keys [graph]}]
  (q/background 1.0)
  (doseq [[_ p] (:nodes graph)]
    (cq/circle p 4.0))
  (doseq [{:keys [p q]} (:edges graph)]
    (q/line (get (:nodes graph) p)
            (get (:nodes graph) q))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition graph-traces
  {:created-at "2024-01-06"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
