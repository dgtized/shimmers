(ns shimmers.sketches.curvature-of-space
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.space-colonization :as colonize]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce defo (debug/state))

(defn build-tree [bounds source attractors]
  (-> {:bounds bounds
       :branches [(colonize/make-root source (gv/vec2))]
       :attractors attractors
       :influence-distance 96
       :prune-distance 6
       :segment-distance 4
       :snap-theta 0
       :jitter (constantly (gv/vec2))}
      colonize/create-tree
      colonize/grow-tree))

(defn tree->branch-points [{:keys [branches]}]
  (->> (group-by :parent branches)
       (keep (fn [[parent-idx children]]
               (when (> (count children) 1)
                 parent-idx)))
       (map #(nth branches %))))

(defn tree->leaf-points [{:keys [branches]}]
  (let [parents (group-by :parent branches)]
    (->> branches
         (map-indexed vector)
         (keep (fn [[i b]] (when (not (get parents i)) b))))))

(defn tree->paths [{:keys [branches] :as tree}]
  (let [branch-points (set (tree->branch-points tree))]
    (->> (map-indexed vector branches)
         (reduce (fn [paths [idx branch]]
                   (let [{:keys [parent]} branch]
                     (if-let [ancestors (get paths parent)]
                       (let [cut (last ancestors)]
                         (if (contains? branch-points cut)
                           (-> paths
                               (assoc idx (vector cut branch)))
                           (-> paths
                               (dissoc parent)
                               (assoc idx (conj ancestors branch)))))
                       (assoc paths idx (vector branch)))))
                 {})
         (keep (fn [[_ branches]] (when (> (count branches) 1) branches)))
         (map #(map :position %)))))

(defn gen-points [n]
  (let [circle (gc/circle (rv 0.5 0.5) (* height 0.45))
        c-left (gc/circle (rv 0.25 0.5) (* height 0.25))
        c-right (gc/circle (rv 0.75 0.5) (* height 0.25))]
    (concat (repeatedly (int (* 0.4 n)) #(geometry/random-point-in-circle circle dr/random))
            (repeatedly (int (* 0.3 n)) #(geometry/random-point-in-circle c-left dr/random))
            (repeatedly (int (* 0.3 n)) #(geometry/random-point-in-circle c-right dr/random)))))

(defn point-path [{:keys [points]}]
  (->> (rest points)
       (map (fn [p] [:L p]))
       (into [[:M (first points)]])
       csvg/path))

(defn shapes [bounds]
  (reset! defo {})
  (let [points (gen-points 128)
        tree (debug/time-it defo [:build-tree] (build-tree bounds (rv 0.5 0.5) points))
        branch-points (tree->branch-points tree)
        leaves (tree->leaf-points tree)
        paths (debug/time-it defo [:tree->paths] (tree->paths tree))]
    (swap! defo assoc
           :branches (count (:branches tree))
           :branch-points (count branch-points)
           :leaves (count leaves)
           :paths (count paths))
    (svg/group {}
               (svg/group {} (for [path paths]
                               (-> (gl/linestrip2 path)
                                   (lines/simplify-line 0.5)
                                   point-path)))
               (svg/group {} (map #(svg/circle (:position %) 2) branch-points))
               (svg/group {} (map #(svg/circle (:position %) 2) leaves))
               (svg/group {}
                          (map #(g/translate (rect/rect 3) %) points)))))

(defn scene []
  (let [bounds (rect/rect 0 0 width height)]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :fill "white"
               :stroke-width 0.8}
              (shapes bounds))))

(sketch/definition curvature-of-space
  {:created-at "2022-01-18"
   :type :svg
   :tags #{:deterministic}}
  (-> scene
      (view-sketch/page-for :curvature-of-space (partial debug/display defo))
      (ctrl/mount "sketch-host")))
