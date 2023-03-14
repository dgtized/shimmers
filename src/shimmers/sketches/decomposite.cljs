(ns shimmers.sketches.decomposite
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn extend-line [p q len]
  (let [theta (g/heading (tm/- q p))]
    (gl/line2 (v/-polar p len theta)
              (v/+polar q len theta))))

(defn decompose [polygon lines]
  (reduce (fn [polygons line]
            (mapcat (fn [poly]
                      (->> line
                           (lines/cut-polygon poly)
                           (filter (fn [sp] (and (> (count (:points sp)) 0)
                                                (> (g/area sp) 0))))))
                    polygons))
          [polygon]
          lines))

(defn regular-shape [container]
  (let [n (dr/rand-nth [3 4 5 6])
        center (g/centroid container)
        size (apply min (map (partial g/dist center) (g/vertices container)))]
    (-> (poly/regular-n-gon n (* 0.25 size))
        (g/rotate (dr/random-tau))
        (g/translate (tm/+ center (dr/jitter (* 0.25 size)))))))

(defn longest-side [container]
  (max (g/width container) (g/height container)))

(defn break-apart [container depth]
  (if (< (g/area container) 1500)
    [container]
    (let [shape (regular-shape container)
          lines (map (fn [[p q]] (extend-line p q (longest-side container)))
                     (g/edges shape))]
      (into [(vary-meta shape assoc :fill "#000")]
            (dr/mapcat-random-sample
             (constantly (/ 0.75 (inc depth)))
             (fn [child] (break-apart child (inc depth)))
             (remove (fn [poly] (g/contains-point? shape (g/centroid poly)))
                     (decompose container lines)))))))

(defn shapes [bounds]
  (break-apart bounds 0))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height))))

(sketch/definition decomposite
  {:created-at "2023-03-14"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :decomposite)
              "sketch-host"))
