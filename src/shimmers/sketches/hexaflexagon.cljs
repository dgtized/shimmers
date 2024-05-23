(ns shimmers.sketches.hexaflexagon
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   ;; [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn triangle-strip [pos dir length n]
  (for [i (range n)
        :let [p (v/+polar pos (* length (tm/floor (+ 0.5 (/ i 2.0)))) 0)]]
    (if (even? i)
      (gt/triangle2 p (v/+polar p length 0) (v/+polar p length dir))
      (gt/triangle2 (v/+polar p length (* 2 dir)) p (v/+polar p length dir)))))

(defn shapes []
  (for [[s l] (mapv vector
                    (concat (triangle-strip (rv 0.05 0.5) (/ eq/TAU 6) (/ (* width 0.8) 4.5) 9)
                            (triangle-strip (rv 0.05 0.5) (- (/ eq/TAU 6)) (/ (* width 0.8) 4.5) 9))
                    (cycle [1 1 2 2 3 3]))]
    (csvg/group {} s
      (csvg/center-label (g/centroid s) (str l) {}))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     ;; [view-sketch/generate :hexaflexagon]
     [:div.readable-width]]))

(sketch/definition hexaflexagon
    {:created-at "2024-05-21"
     :tags #{}
     :type :svg}
  (ctrl/mount page))
