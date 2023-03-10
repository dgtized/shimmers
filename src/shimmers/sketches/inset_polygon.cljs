(ns shimmers.sketches.inset-polygon
  (:require
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; all negatives indicate clockwise, but a mix probably means a switch back with
;; a counter-clockwise loop
(defn orientation [points]
  (->> points
       cs/triplet-cycle
       (mapv (fn [[a b c]] (tm/sign (v/orient2d a b c))))))

(defn polygon-state [inset]
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
           (poly-detect/self-intersection-polygons inset))
     :orient-poly (orientation (g/vertices poly))
     :orient-inset (orientation (g/vertices inset))}))

(defn scene [{:keys [polygon inset]}]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"}
    [(csvg/group {:stroke "blue"} polygon)
     (csvg/group {:stroke "red"} inset)]))

(defn page []
  (let [ui-state (ctrl/state {:inset 100})]
    (fn []
      (let [state (polygon-state (:inset @ui-state))]
        [:div
         [:div.canvas-frame [(partial scene state)]]
         [:div.explanation.contained
          [:div
           [:div.ui-controls
            (ctrl/slider ui-state (fn [x] (str "Inset " x)) [:inset] [-100 200 1])]
           [:div
            (debug/pre-edn state)]]]]))))

(sketch/definition inset-polygon
  {:created-at "2023-02-27"
   :type :svg
   :tags #{:demo}}
  (ctrl/mount (page) "sketch-host"))
