(ns shimmers.sketches.graph-traces
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn now []
  (/ (q/millis) 1000.0))

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

(defn out-edges [{:keys [edges]} node]
  (let [conns (filter (fn [{:keys [p q]}] (or (= node p) (= node q))) edges)]
    (seq (disj (set/union (set (map :p conns)) (set (map :q conns))) node))))

(defn new-ship [graph node now]
  {:p node :q (dr/rand-nth (out-edges graph node))
   :t0 now :t1 (+ now (dr/random 1 4))})

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [graph (new-graph)
        now (now)]
    {:graph graph
     :t now
     :ships (repeatedly 3 (fn [] (let [node (dr/rand-nth (keys (:nodes graph)))]
                                  (new-ship graph node now))))}))

(defn update-ships [ships {:keys [nodes] :as graph} now]
  (for [{:keys [p q t0 t1] :as ship} ships]
    (if (>= now t1)
      (new-ship graph q now)
      (assoc ship :pos (tm/mix (nodes p) (nodes q) (/ (- now t0) (- t1 t0)))))))

(defn update-state [{:keys [graph] :as state}]
  (update state :ships update-ships graph (now)))

(defn draw [{:keys [graph ships]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/fill 0.0)
  (doseq [[_ p] (:nodes graph)]
    (cq/circle p 3.0))
  (doseq [{:keys [p q]} (:edges graph)]
    (q/line (get (:nodes graph) p)
            (get (:nodes graph) q)))
  (doseq [{:keys [pos]} ships]
    (when pos
      (cq/circle pos 4.0))))

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