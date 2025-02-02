(ns shimmers.sketches.continuity
  (:require
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn generate-path [bounds n _r]
  (let [pts (rp/poisson-disc-sampling bounds (inc n))
        voronoi (delvor/voronoi-cells pts bounds)
        start (g/centroid (dr/rand-nth voronoi))]
    (loop [pos start
           cells (remove (fn [cell] (g/contains-point? cell start))
                         voronoi)
           path []]
      (if (or (empty? cells) (>= (count path) n))
        {:path path
         :cells voronoi}
        (let [nearby (sort-by (fn [cell] (g/dist-squared pos (g/centroid cell))) cells)
              cell (dr/rand-nth (take 2 nearby))
              pos' (g/centroid cell)]
          (recur pos'
                 (remove (fn [cell] (g/contains-point? cell pos')) nearby)
                 (conj path pos)))))))

(defn curved-path [points]
  (concat [[:M (first points)]]
          (->> (rest points)
               (partition-all 3 2)
               (mapcat (fn [group]
                         (into [[:L (first group)]]
                               (map (fn [p] [:T p]) (rest group))))))))

(defn shapes [bounds]
  (let [{:keys [path cells]} (generate-path bounds 12 96)]
    (concat [(csvg/group {:stroke-width 2.5}
               (csvg/path (curved-path path)))
             (csvg/group {:fill "none"}
               (mapv (fn [p] (gc/circle p 3.0)) path))]
            cells)))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes (g/scale-size (csvg/screen width height) 0.8))))

(defn explanation [_]
  [:div
   [:p "Genuary 2025 Day 25 - One line that may or may not intersect itself"]
   [:p "Experimenting with a random walk through the centroids of voronoi cells."]])

(sketch/definition continuity
  {:created-at "2025-01-25"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (usvg/page (assoc sketch-args
                     :explanation explanation)
              scene)))
