(ns shimmers.sketches.cracked-playa
  (:require
   [shimmers.algorithm.chaikin :as chaikin]
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.string :as scs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
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

;; TODO: add rough edges to each polygon?
;; TODO: look at smoothing polygons first, but gp/smooth does something else
(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        seed (gv/vec2 (dr/random 100) (dr/random 100))
        points (pds/generate-dynamic bounds 10 [8 64] (partial dr/noise-at-point seed 0.005))
        cells (delvor/voronoi-cells points bounds)]
    (->> cells
         (mapcat (fn [cell]
                   (let [width (dr/random 0.5 4)
                         inset (poly-detect/inset-polygon cell width)]
                     (poly-detect/split-self-intersection inset))) )
         (filter (fn [s] (> (g/area s) 0)))
         (map (fn [{:keys [points]}]
                ;; TODO: make this proportional to size?
                (let [ratio (Math/abs (dr/gaussian 0.0 0.12))
                      iters (dr/random-int 1 4)
                      poly (gp/polygon2 (chaikin/chaikin ratio true iters points))
                      centroid-noise (dr/noise-at-point (tm/* seed 1.5) 0.005 (g/centroid poly))]
                  (vary-meta poly assoc :fill
                             (color/css-hsl (+ 0.075 (* 0.25 centroid-noise)) 0.4 0.75))))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (apply list (shapes))))

(defonce sink (debug/state []))
(defn profile-summary [sink]
  (let [spans @sink
        started (apply min (map :start spans))
        complete (apply max (map :stop spans))
        total-duration (- complete started)
        spans (for [{:keys [start stop] :as span} spans
                    :let [duration (- stop start)]]
                (assoc span
                       :duration duration
                       :percent (/ duration total-duration)))]
    [:ul {:key "profile-cracked-playa"}
     (for [{:keys [desc duration percent]} (sort-by :start spans)]
       [:li {:key (str "li-" (name desc))} (scs/format "%s %.1f ms (%1.1f%%)" (name desc) duration (* 100 percent))])
     [:li {:key "li-total"} (scs/format "Total %.1f ms" total-duration)]]))

(defn page []
  [:div
   [:div.canvas-frame [scene]]
   [:div.explanation
    [:div.flexcols
     [:div [view-sketch/generate :cracked-playa]]
     #_[profile-summary sink]]]])

(sketch/definition cracked-playa
  {:created-at "2022-04-03"
   :type :svg
   :taps [(debug/profile-to sink)]
   :tags #{:deterministic}}
  (ctrl/mount page "sketch-host"))
