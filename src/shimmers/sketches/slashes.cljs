(ns shimmers.sketches.slashes
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        hatch (partial clip/variable-hatching bounds)]
    (concat (hatch (tm/random 5.0 6.0) 0
                   (int (tm/random 6 16)) (constantly 10) (constantly 1))
            (hatch (tm/random 5.0 6.0) (* width 0.8)
                   10 #(tm/random 5 15) #(tm/random 0.8 6))
            (hatch (tm/random 3.5 4.5) (* width 0.4)
                   (int (tm/random 4 16)) #(tm/random 5 15) #(tm/random 0.5 4))
            (hatch (tm/random 3.5 4.5) (* width 1.3)
                   8 #(tm/random 5 15) #(tm/random 0.5 2)))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 1.0}
     (mapv (fn [{:keys [width] :as line}]
             (vary-meta line assoc :stroke-width width))
           (shapes)))))

(sketch/definition slashes
  {:created-at "2021-08-20"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :slashes)
              "sketch-host"))
