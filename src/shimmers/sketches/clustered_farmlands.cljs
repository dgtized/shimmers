(ns shimmers.sketches.clustered-farmlands
  (:require [kixi.stats.distribution :as ksd]
            [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.bezier :as bezier]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn randnorm [mu sd]
  (ksd/draw (ksd/normal {:mu mu :sd sd})))

(def width 800)
(def height 800)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-offsets-spaced [lower upper spacing]
  (for [o (range lower upper spacing)]
    (+ o (* spacing (randnorm 0 0.1)))))

(defn generate-houses
  [road]
  (fn [[y1 y2]]
    (let [mid1 (geom/point-at road y1)
          mid2 (geom/point-at road y2)
          units (* width 0.02)
          normal (-> (tm/- mid2 mid1)
                     geom/normal
                     tm/normalize
                     (tm/* (* units 1)))]
      [{:pos (tm/+ (tm/mix mid1 mid2 0.5)
                   normal
                   (gv/vec2 (randnorm 0 (* 0.1 units)) (randnorm 0 (* 0.2 units))))
        :heading (geom/heading-xy normal)}
       {:pos (tm/+ (tm/mix mid1 mid2 0.5)
                   (tm/- normal)
                   (gv/vec2 (randnorm 0 (* 0.1 units)) (randnorm 0 (* 0.2 units))))
        :heading (geom/heading-xy normal)}])))

;; TODO curve the "road" and add houses + lots along it, spaced at some interval
;; curve the field rows with dampened displacement along the path?
;; Break up the field rows occasionally and sometimes match the rows above and below
;; Add varying green levels per row
(defn scene []
  (let [spacing 0.05
        road (bezier/auto-spline2 [(r (randnorm 0.5 0.1) (- (* 2 spacing)))
                                   (r (randnorm 0.5 0.01) 0.3)
                                   (r (randnorm 0.5 0.01) 0.7)
                                   (r (randnorm 0.5 0.1) (+ 1 (* 2 spacing)))])
        ;; Trying to make them line up, but to be fields I think they have to be
        ;; separate to fill later
        rows (for [y (random-offsets-spaced 0 1 spacing)
                   :let [mid (geom/point-at road y)]
                   :when mid]
               [y (bezier/auto-spline2 [(r 0 y) mid (r 1 y)])])
        ;; TODO: stay out of the road, vary shapes or multiple buildings, and build on both sides
        houses (mapcat (generate-houses road) (partition 2 1 (map first rows)))]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 0.5}
              (svg/polyline (geom/sample-uniform road 10 true)
                            {:stroke-width 5})
              (for [[y row] rows]
                (svg/polyline (geom/sample-uniform row 10 true)
                              {:key (str "r" y)}))
              (for [[i {:keys [pos heading]}] (map-indexed vector houses)]
                (svg/group {:transform (csvg/rotate heading pos)
                            :key (str "house" i)}
                           (svg/rect pos 5 5))))))

(defn page []
  [:div (scene)])

(defn ^:export run-sketch []
  ;; 20210510
  (ctrl/mount page "svg-host"))
