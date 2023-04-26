(ns shimmers.sketches.decomposite
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
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

(defn longest-edge [polygon]
  (apply gl/line2 (apply max-key (fn [[p q]] (g/dist p q)) (g/edges polygon))))

;; FIXME: does this actually work? Concept is to find angle of longest edge and
;; rotate container to align to that edge as x axis?
(defn axis-aligned [polygon]
  (let [longest (longest-edge polygon)
        theta (g/heading longest)]
    (-> polygon
        g/center
        (g/rotate (- theta)))))

;; FIXME: This scaling to fit is not always working, sometimes it pokes out
(defn regular-shape [container]
  (let [n (dr/rand-nth [3 4 5 6])
        aligned (axis-aligned container)
        center (g/centroid aligned)
        ;; Is this the same as closest edge distance?
        size (min (apply min (map (partial g/dist center) (g/vertices aligned)))
                  (* 0.5 (g/height aligned))
                  (* 0.5 (g/width aligned)))]
    (-> (poly/regular-n-gon n (* 0.25 size))
        (g/rotate (dr/random-tau))
        (g/translate (tm/+ (g/centroid container) (dr/jitter (* 0.33 size)))))))

(defn longest-side [container]
  (max (g/width container) (g/height container)))

(defn cut-outs [shape]
  (cond (< (g/area shape) 160)
        [(vary-meta shape assoc :fill "#000")]
        (< (g/area shape) 1000)
        (let [s1 (poly-detect/inset-polygon shape (dr/random 2.0 4.0))
              s2 (poly-detect/inset-polygon shape (dr/random 4.0 8.0))]
          [s1 s2])
        :else
        (for [o (dr/gaussian-range (/ 500 (g/area shape)) (dr/random 0.01 0.15))]
          (-> shape
              (poly-detect/inset-polygon (* 0.4 o (Math/sqrt (g/area shape))))
              (geometry/rotate-around-centroid (dr/gaussian 0.0 (* 0.015 o)))))))

(defn break-apart [container depth]
  (if (< (g/area container) 1500)
    [container]
    (let [shape (regular-shape container)
          lines (map (fn [[p q]] (extend-line p q (longest-side container)))
                     (g/edges shape))]
      (into (cut-outs shape)
            (dr/mapcat-random-sample
             (constantly (/ 0.75 (inc depth)))
             (fn [child] (break-apart child (inc depth)))
             (remove (fn [poly] (g/contains-point? shape (g/centroid poly)))
                     (lines/slice-polygons [container] lines)))))))

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
