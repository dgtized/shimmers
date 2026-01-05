(ns shimmers.sketches.variations-on-molnar
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [thi.ng.geom.polygon :as gp]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 6)
  (let [region (g/center (rect/rect 0 0 (/ (q/width) 1.1) (/ (q/width) 1.6))
                         (cq/rel-vec 0.5 0.5))
        cells (g/subdivide region {:rows 16 :cols 24})]
    {:region region
     :cells cells
     :cell-size (g/width (first cells))}))

(defn update-state [state]
  state)

(defn draw [{:keys [region cells cell-size]}]
  (q/background 1.0)
  (q/no-fill)
  (qdg/draw (gp/polygon2 (gp/inset-polygon (g/vertices region) 4.0)))
  (qdg/draw region)
  (doseq [cell cells]
    (qdg/draw cell)
    (doseq [inset (reductions +
                              (repeatedly (dr/random-int 1 (int (/ cell-size 4)))
                                          #(dr/random -0.75 -3.25)))]
      (let [polygon (gp/polygon2 (gp/inset-polygon (g/vertices cell) inset))]
        (qdg/draw (geometry/rotate-around-centroid polygon (dr/random -0.05 0.05)))))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.centered.readable-width
    [:p "Genuary 2026 - Day 4 - Low Res"]
    [:p "Infinite Vera Molnár squares"]]])

(sketch/definition variations-on-molnar
  {:created-at "2026-01-04"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))
