(ns shimmers.sketches.clustered-farmlands
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 800)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-offsets-spaced [lower upper spacing]
  (for [o (range lower upper spacing)]
    (+ o (* spacing (dr/gaussian 0 0.1)))))

(defn generate-houses
  [road]
  (fn [[y1 y2]]
    (let [mid1 (g/point-at road y1)
          mid2 (g/point-at road y2)
          units (* width 0.02)
          normal (-> (tm/- mid2 mid1)
                     g/normal
                     tm/normalize
                     (tm/* (* units 1)))]
      [{:pos (tm/+ (tm/mix mid1 mid2 0.5)
                   normal
                   (gv/vec2 (dr/gaussian 0 (* 0.1 units)) (dr/gaussian 0 (* 0.2 units))))
        :heading (g/heading-xy normal)}
       {:pos (tm/+ (tm/mix mid1 mid2 0.5)
                   (tm/- normal)
                   (gv/vec2 (dr/gaussian 0 (* 0.1 units)) (dr/gaussian 0 (* 0.2 units))))
        :heading (g/heading-xy normal)}])))

;; TODO curve the "road" and add houses + lots along it, spaced at some interval
;; curve the field rows with dampened displacement along the path?
;; Break up the field rows occasionally and sometimes match the rows above and below
;; Add varying green levels per row
(defn scene []
  (let [spacing 0.05
        road (bezier/auto-spline2 [(r (dr/gaussian 0.5 0.1) (- (* 2 spacing)))
                                   (r (dr/gaussian 0.5 0.01) 0.3)
                                   (r (dr/gaussian 0.5 0.01) 0.7)
                                   (r (dr/gaussian 0.5 0.1) (+ 1 (* 2 spacing)))])
        ;; Trying to make them line up, but to be fields I think they have to be
        ;; separate to fill later
        road-start (g/point-at road 0.0)
        rows (for [y (random-offsets-spaced 0.01 0.99 spacing)
                   :let [mid (g/point-at road y)]
                   :when mid]
               (let [normal (-> (tm/- road-start mid)
                                g/normal
                                tm/normalize
                                (tm/* (* width 0.15)))
                     path [(r 0 y)
                           (tm/- mid normal)
                           (tm/+ mid normal)
                           (r 1 y)]]
                 [y (bezier/auto-spline2 path)]))
        ;; TODO: stay out of the road, vary shapes or multiple buildings, and build on both sides
        houses (mapcat (generate-houses road) (partition 2 1 (map first rows)))]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 0.5}
              (svg/polyline (g/sample-uniform road 10 true)
                            {:stroke-width 5})
              (for [[_ row] rows]
                (svg/polyline (g/sample-uniform row 10 true)))
              (for [{:keys [pos heading]} houses]
                (svg/group {:transform (csvg/rotate heading pos)}
                           (svg/rect pos 5 5))))))

(sketch/definition clustered-farmlands
  {:created-at "2021-05-10"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount (view-sketch/page-for scene :clustered-farmlands)
              "sketch-host"))
