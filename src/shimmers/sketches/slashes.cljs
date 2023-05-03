(ns shimmers.sketches.slashes
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        hatch (partial clip/variable-hatching bounds)]
    (concat (hatch (dr/random 5.0 6.0) 0
                   (int (dr/random 6 16)) (constantly 10) (constantly 1))
            (hatch (dr/random 5.0 6.0) (* width 0.8)
                   10 #(dr/random 5 15) #(dr/random 0.8 6))
            (hatch (dr/random 3.5 4.5) (* width 0.4)
                   (int (dr/random 4 16)) #(dr/random 5 15) #(dr/random 0.5 4))
            (hatch (dr/random 3.5 4.5) (* width 1.3)
                   8 #(dr/random 5 15) #(dr/random 0.5 2)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (mapv (fn [{:keys [width] :as line}]
            (vary-meta line assoc :stroke-width width))
          (shapes))))

(sketch/definition slashes
  {:created-at "2021-08-20"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :slashes)))
