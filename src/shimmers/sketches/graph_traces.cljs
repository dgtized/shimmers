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
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils.intersect :as gisec]
   [thi.ng.math.core :as tm]))

(defn now []
  (/ (q/millis) 1000.0))

(defn planar? [nodes edges [p q]]
  (not-any?
   (fn [[a b]]
     (and (not (or (= a p) (= b p) (= a q) (= b q)))
          (= :intersect (:type (gisec/intersect-line2-line2?
                                (nodes p) (nodes q)
                                (nodes a) (nodes b))))))
   edges))

(defn planar-edges [nodes]
  (->> nodes
       keys
       cs/all-pairs
       ;; order by shortest edge
       (sort-by (fn [[p q]] (g/dist (get nodes p) (get nodes q))))
       ;; greedy accept planar edges
       (reduce (fn [accepted edge]
                 (if (planar? nodes accepted edge)
                   (conj accepted edge)
                   accepted))
               [])))

(defn new-graph []
  (let [nodes (zipmap (map (comp keyword char) (range 65 (+ 65 26)))
                      (repeatedly 11 #(rp/sample-point-inside (cq/screen-rect 0.85))))]
    {:nodes nodes
     :edges (for [[p q] (planar-edges nodes)]
              {:p p :q q})}))

(defn out-edges [{:keys [edges]} node]
  (let [conns (filter (fn [{:keys [p q]}] (or (= node p) (= node q))) edges)]
    (seq (disj (set/union (set (map :p conns)) (set (map :q conns))) node))))

(defn new-ship [graph fill node now]
  {:p node :q (dr/rand-nth (out-edges graph node))
   :fill fill
   :t0 now :t1 (+ now (dr/random 1 4))})

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [graph (new-graph)
        now (now)]
    {:graph graph
     :t now
     :ships (for [f (range 7)]
              (let [node (dr/rand-nth (keys (:nodes graph)))]
                (new-ship graph [(mod (* f tm/PHI) 1.0) 0.66 0.5 0.5] node now)))}))

(defn contract-edge [{:keys [nodes] :as graph} {:keys [p q]} dt]
  (let [p' (nodes p)
        q' (nodes q)
        dist (g/dist-squared p' q')
        move (tm/* (tm/- q' p') (/ dt dist))]
    (-> graph
        (update-in [:nodes p] tm/+ move)
        (update-in [:nodes q] tm/- move))))

(defn expand-edge [{:keys [nodes] :as graph} {:keys [p q]} dt]
  (let [p' (nodes p)
        q' (nodes q)
        dist (g/dist-squared p' q')
        move (tm/* (tm/- p' q') (/ dt dist))]
    (-> graph
        (update-in [:nodes p] tm/+ move)
        (update-in [:nodes q] tm/- move))))

(defn avoid-edges [graph rect node dt]
  (let [pos (tm/+ ((:nodes graph) node) (dr/jitter 3.0))
        closest (g/closest-point rect pos)
        force (tm/* (tm/- pos closest) (* 0.5 (/ dt (g/dist-squared pos closest))))]
    (if (g/contains-point? rect pos)
      (update-in graph [:nodes node] tm/+ force)
      (assoc-in graph [:nodes node] (g/closest-point (g/scale-size rect 0.98) pos)))))

(defn update-graph [{:keys [nodes edges] :as graph} dt]
  (let [ranked-edges (sort-by (fn [{:keys [p q]}] (g/dist (nodes p) (nodes q))) edges)
        closest (take 12 ranked-edges)
        furthest (take-last 4 ranked-edges)
        box (cq/screen-rect 1.01)]
    (as-> graph graph
      (reduce (fn [g n] (avoid-edges g box n dt)) graph (keys nodes))
      (reduce (fn [g e] (expand-edge g e dt)) graph closest)
      (reduce (fn [g e] (contract-edge g e dt)) graph furthest))))

(defn update-ships [ships {:keys [nodes] :as graph} now]
  (for [{:keys [p q t0 t1 fill] :as ship} ships]
    (if (>= now t1)
      (new-ship graph fill q now)
      (assoc ship :pos (tm/mix (nodes p) (nodes q) (/ (- now t0) (- t1 t0)))))))

(defn update-state [{:keys [t graph] :as state}]
  (let [now (now)]
    (-> state
        (update :graph update-graph (* 500 (- now t)))
        (update :ships update-ships graph t)
        (assoc :t now))))

(defn draw [{:keys [graph ships t]}]
  (let [tv (+ (* 0.7 (eq/unit-sin (* 0.1 t)))
              (* 0.3 (eq/unit-cos (* 0.33 t))))]
    (q/background 1.0 (+ 0.01 (* 0.99 (tm/smoothstep* 0.6 0.8 tv)))))
  (q/ellipse-mode :radius)
  (q/fill 0.0 0.1)
  (q/stroke 0.0 0.1)
  (doseq [[_ p] (:nodes graph)]
    (cq/circle p 3.0))
  (doseq [{:keys [p q]} (:edges graph)]
    (q/line (get (:nodes graph) p)
            (get (:nodes graph) q)))
  (q/stroke 0.0 0.25)
  (q/fill 0.0 0.4)
  (doseq [{:keys [pos fill]} ships]
    (when fill
      (q/fill fill))
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
