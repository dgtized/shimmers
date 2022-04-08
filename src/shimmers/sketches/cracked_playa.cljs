(ns shimmers.sketches.cracked-playa
  (:require
   [shimmers.algorithm.chaikin :as chaikin]
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.math.noise :as noise]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn noise-at-point [p]
  (let [[x y] (tm/* p 0.01)]
    (tm/clamp01 (noise/noise2 x y))))

;; TODO: add rough edges to each polygon?
;; TODO: look at smoothing polygons first, but gp/smooth does something else
(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        points (debug/span-prof :gen-points
                                (pds/generate-dynamic bounds 10 [18 256] noise-at-point))
        cells (debug/span-prof :voronoi
                               (delvor/voronoi-cells points bounds))]
    (debug/span-prof :inset-split
                     (->> (for [cell cells
                                :let [width (dr/random -0.5 -4)
                                      inset (gp/polygon2 (gp/inset-polygon (:points cell) width))]
                                ;;:when (poly-detect/self-intersecting? inset)
                                ]
                            (poly-detect/split-self-intersection inset))
                          (apply concat)
                          (filter (fn [s] (> (g/area s) 0)))
                          (map (fn [{:keys [points]}]
                                 ;; TODO: make this proportional to size?
                                 (let [ratio (Math/abs (dr/gaussian 0.0 0.12))
                                       iters (dr/random-int 1 4)]
                                   (gp/polygon2 (chaikin/chaikin ratio true iters points)))))))))

(defonce sink (debug/state []))

(defn scene []
  (reset! sink [])
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (debug/span-prof :render
                             (apply list (shapes)))))

(sketch/definition cracked-playa
  {:created-at "2022-04-03"
   :type :svg
   :taps [(debug/profile-to sink)]
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :cracked-playa
                                    (fn [] [debug/pre-edn @sink]))
              "sketch-host"))
