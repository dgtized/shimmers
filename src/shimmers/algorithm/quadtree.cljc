(ns shimmers.algorithm.quadtree
  (:require [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.geom.core :as g]))

;; translated from http://bl.ocks.org/patricksurry/6478178
(defn nearest-neighbor
  ([quadtree point]
   (:p (nearest-neighbor quadtree point
                         (let [{[w h] :size} (g/bounds quadtree)]
                           {:d (+ w h) :p nil}))))
  ([quadtree point {:keys [d] :as best}]
   (if-not quadtree
     best
     (let [[x y] point
           {[x0 y0] :p [nw nh] :size} (g/bounds quadtree)]
       (if (or (< x (- x0 d)) (> x (+ x0 nw d))
               (< y (- y0 d)) (> y (+ y0 nh d)))
         best
         (let [best' (or (when-let [node-point (g/get-point quadtree)]
                           (let [d' (g/dist point node-point)]
                             (when (< d' d)
                               {:d d' :p node-point})))
                         best)]
           (reduce (fn [better child]
                     (nearest-neighbor child point better))
                   best' (spatialtree/get-children quadtree))))))))
