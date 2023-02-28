(ns shimmers.sketches.inset-polygon
  (:require
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn sketch-polygon [inset]
  (let [poly (gp/polygon2 (rv 0.2 0.2)
                          (rv 0.7 0.5)
                          (rv 0.5 0.7)
                          (rv 0.1 0.6))
        inset (poly-detect/inset-polygon poly inset)]
    {:polygon poly
     :inset inset
     :self-intersect (poly-detect/self-intersecting? inset)
     :self-intersection-polygons
     (mapv (fn [poly] [poly :clockwise (poly-detect/clockwise-polygon? (g/vertices poly))])
           (poly-detect/self-intersection-polygons inset))}))

(defn shapes [ui-state]
  (let [{:keys [polygon inset]} (sketch-polygon (:inset @ui-state))]
    [(csvg/group {:stroke "blue"} polygon)
     (csvg/group {:stroke "red"} inset)]))

(defn scene [ui-state]
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"}
             (shapes ui-state))))

(defn page []
  (let [ui-state (ctrl/state {:inset 100})]
    (fn []
      [:div
       [:div.canvas-frame [(partial scene ui-state)]]
       [:div.explanation.contained
        [:div
         [:div.ui-controls
          (ctrl/slider ui-state (fn [x] (str "Inset " x)) [:inset] [-100 200 1])]
         [:div
          (debug/pre-edn (sketch-polygon (:inset @ui-state)))]]]])))

(sketch/definition inset-polygon
  {:created-at "2023-02-27"
   :type :svg
   :tags #{}}
  (ctrl/mount (page) "sketch-host"))
