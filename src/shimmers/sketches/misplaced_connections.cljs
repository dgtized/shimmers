(ns shimmers.sketches.misplaced-connections
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.line :as gl]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn find-cuts [offsets]
  (let [a (dr/rand-nth (butlast offsets))]
    [a (dr/rand-nth (drop-while #(<= % a) offsets))]))

;; TODO: for v-offsets maybe detect if line actually meets there?
;; or go through and trim lines that don't connect?
;; experiment with multiple gaps?
(defn shapes []
  (let [h-offsets (dr/gaussian-range 0.04 0.03)
        v-offsets (dr/gaussian-range 0.03 0.03)]
    (concat
     (mapcat concat
             (for [h h-offsets]
               (if (dr/chance 0.66)
                 (let [[a b] (find-cuts v-offsets)]
                   [(gl/line2 (rv 0.0 h) (rv a h))
                    (gl/line2 (rv b h) (rv 1.0 h))])
                 [(gl/line2 (rv 0.0 h) (rv 1.0 h))])))
     (mapcat concat
             (for [v v-offsets]
               (if (dr/chance 0.66)
                 (let [[a b] (find-cuts h-offsets)]
                   [(gl/line2 (rv v 0.0) (rv v a))
                    (gl/line2 (rv v b) (rv v 1.0))])
                 [(gl/line2 (rv v 0.0) (rv v 1.0))]))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes)))

(sketch/definition misplaced-connections
  {:created-at "2023-03-10"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :misplaced-connections)
              "sketch-host"))
