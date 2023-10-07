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

(def radius (* 0.45 height))

(defn ring [seed r displace]
  (for [t (dr/gaussian-range 0.01 0.01)]
    (let [p (v/polar (* radius r ) (* t eq/TAU))
          noise (dr/noise-at-point-01 seed 0.0035 p)]
      (tm/+ p (v/polar displace (* eq/TAU noise))))))

;; TODO make n triangles proportional to ring size
(defn shapes [seed]
  (let [rings (mapv (fn [r] (gp/polygon2 (vec (ring seed (- r 0.05) (* 0.025 radius (+ 1 r))))))
                    (dr/gaussian-range 0.1 0.005))]
    (into rings
          (map (fn [[i [r0 r1]]]
                 (let [n (int (* (dr/random-int 6 36)
                                 (+ 1 (* 4 (/ (float i) (count rings))))))
                       inner?
                       (dr/weighted [[odd? 2]
                                     [(fn [t] (> (mod t 3) 0)) 1]
                                     [(fn [t] (> (mod t 4) 1)) 2]
                                     [(fn [t] (and (< (mod t 6) 4) (odd? t))) 1]
                                     [(fn [t] (and (< (mod t 5) 3) (odd? t))) 1]])]
                   (gp/polygon2 (for [t (range (inc n))]
                                  (g/point-at (if (inner? t) r0 r1)
                                              (/ (float t) n))))))
               (map-indexed vector (partition 2 1 rings))))))

(defn scene [seed]
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes seed))))

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
