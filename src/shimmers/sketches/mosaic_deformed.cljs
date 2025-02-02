(ns shimmers.sketches.mosaic-deformed
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn ring [seed r displace]
  (for [t (dr/gaussian-range 0.01 0.01)]
    (let [p (v/polar r (* t eq/TAU))
          noise (dr/noise-at-point-01 seed 0.0035 p)]
      (tm/+ p (v/polar displace (* eq/TAU noise))))))

(defn ring-pattern []
  (dr/weighted
   [[odd? 2]
    [(fn [t] (> (mod t 3) 0)) 1]
    [(fn [t] (> (mod t 4) 1)) 2]
    [(fn [t] (and (< (mod t 6) 4) (odd? t))) 1]
    [(fn [t] (and (< (mod t 5) 3) (odd? t))) 1]]))

(defn fill-color [base]
  (csvg/hsl ((dr/weighted
              [[(fn [] (+ base (dr/random 0.12))) 4.0]
               [(fn [] (+ base 0.5 (dr/random 0.06))) 1.0]]))
            (dr/random 0.5 0.8)
            (tm/clamp01 (dr/gaussian 0.85 0.12))))

(defn shapes [bounds seed base-color]
  (let [radius (* 0.45 (geometry/min-axis bounds))
        rings (mapv (fn [r]
                      (vary-meta
                       (gp/polygon2 (ring seed
                                          (* radius (- r 0.05))
                                          (* radius 0.025 (+ 1 r))))
                       assoc :fill "none"))
                    (dr/gaussian-range 0.075 0.02))]
    (->> rings
         (partition 2 1)
         (map-indexed vector)
         (map (fn [[i [r0 r1]]]
                (let [n (int (* (dr/random-int 6 36)
                                (+ 1 (* 4 (/ (float i) (count rings))))))
                      inner? (ring-pattern)]
                  (vary-meta
                   (gp/polygon2 (for [t (range (inc n))]
                                  (g/point-at (if (inner? t) r0 r1)
                                              (/ (float t) n))))
                   assoc :stroke-width
                   (dr/weighted {0.25 4 0.5 6 0.75 4 1.0 2 1.5 1})
                   :fill (fill-color base-color)))))
         (into rings))))

(defn mosaic [seed bounds base-color]
  (csvg/group {:transform (csvg/translate (:p bounds))}
    (reverse (shapes bounds seed base-color))))

(defn place [px py pw ph]
  (let [[x y] (rv px py)
        w (* pw width)
        h (* ph height)]
    (rect/rect x y w h)))

(defn layout [seed base-color]
  (case (dr/weighted {:pair-left 1
                      :pair-right 1
                      :solo 1})
    :pair-left
    [(mosaic seed (place 0.3 0.25 0.75 0.5) base-color)
     (mosaic seed (place 0.3 0.75 0.75 0.5) base-color)
     (mosaic seed (place 0.7 (+ 0.45 (dr/random 0.1)) 0.8 0.8) base-color)]
    :pair-right
    [(mosaic seed (place 0.7 0.25 0.75 0.5) base-color)
     (mosaic seed (place 0.7 0.75 0.75 0.5) base-color)
     (mosaic seed (place 0.3 (+ 0.45 (dr/random 0.1)) 0.8 0.8) base-color)]
    :solo
    [(mosaic seed (place (dr/random 0.4 0.6) (dr/random 0.45 0.55) 1.0 1.0)
             base-color)]))

(defn scene [seed base-color]
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (layout seed base-color)))

(defn page []
  (let [seed (dr/noise-seed)
        base-color (dr/random)]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene seed base-color]]
       [view-sketch/generate :mosaic-deformed]
       [:div.readable-width
        [:p "Create concentric rings, deformed by simplex noise at each sample
       point. From these initial rings, create polygons connecting each
       consecutive pair of rings using different patterns. With an odd/even
       pattern it creates a serrated triangle edged polygon, but some patterns
       skip or bias towards one ring creating other outlines. Each of these
       polygons is assigned a random color near a common hue. Occasionally, the
       common hue is rotated 180 degrees around the color wheel to add some
       accents. Each polygon is then drawn in reverse order, outside-in,
       ensuring the fill overlaps the inside of the previous ringed polygon."]]])))

(sketch/definition mosaic-deformed
  {:created-at "2023-10-04"
   :tags #{}
   :type :svg}
  (ctrl/mount (page)))
