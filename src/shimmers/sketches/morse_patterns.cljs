(ns shimmers.sketches.morse-patterns
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg]
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
  (let [bounds (rect/rect 0 0 width height)]
    (clip/variable-hatching bounds 0.3 0 120
                            (dr/cyclic [2 4 8 12])
                            (dr/cyclic [0.5 2 3 2 1]))))

;; FIXME: handle large gaps and overlapping lines
(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (for [[i shape] (map-indexed vector (shapes))]
              (vary-meta shape assoc :key (str "l" i)
                         :stroke-width (:width shape)))))

(sketch/definition morse-patterns
  {:created-at "2021-12-02"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :morse-patterns)
              "sketch-host"))
