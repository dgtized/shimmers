(ns shimmers.sketches.mosaic-deformed
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
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

(defn shapes [[width height] seed]
  (let [radius (* 0.45 (min width height))
        rings (mapv (fn [r] (gp/polygon2 (ring seed
                                              (* radius (- r 0.05))
                                              (* radius 0.025 (+ 1 r)))))
                    (dr/gaussian-range 0.075 0.02))
        base (dr/random)]
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
                   :fill (fill-color base)))))
         (into rings))))

(defn scene [seed]
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (reverse (shapes [width height] seed)))))

(defn page []
  (let [seed (gv/vec2 (dr/random 100) (dr/random 100))]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene seed]]
       [:div.flexcols
        [view-sketch/generate :mosaic-deformed]]])))

(sketch/definition mosaic-deformed
  {:created-at "2023-10-04"
   :tags #{}
   :type :svg}
  (ctrl/mount (page)))
