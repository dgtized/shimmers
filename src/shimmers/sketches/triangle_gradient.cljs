(ns shimmers.sketches.triangle-gradient
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)

(defn contained-by? [f bounds shape]
  (f (fn [v] (when (g/contains-point? bounds v)
              v))
     (g/vertices shape)))

(defn invert [x]
  (- 1.0 x))

(defn shapes []
  (let [circle (gc/circle (* width 0.5) (* height 0.5) (* width 0.45))
        r (* width 0.05)
        template (dr/rand-nth [(triangle/inscribed-equilateral (gc/circle r) 0)
                               (g/center (gt/triangle2 [0 0] [r 0] [0 r]))
                               (g/center (gt/triangle2 [0 0] [r r] [(* r 0.75) (* r 0.125)]))])
        scale (dr/random 0.1 0.6)
        dir-s (dr/weighted {invert 1
                            identity 3
                            (constantly scale) 1
                            #(dr/random 0.1 0.3) 1})
        dir-x (dr/weighted {invert 1
                            identity 3})
        generate (fn []
                   (let [x (dir-x (math/pow (dr/random) 0.4))
                         y (dr/random 0.15 0.85)]
                     (-> template
                         (g/scale-size (+ 0.15 (dir-s (math/pow x 1.4))))
                         (g/rotate (dr/random 0 tm/TWO_PI))
                         (g/translate (gv/vec2 (* width x) (* height y))))))]
    (->> (repeatedly 4000 generate)
         (filter (partial contained-by? some circle))
         (gu/fit-all-into-bounds (rect/rect 0 0 width height)))))

(defn scene []
  (csvg/svg-timed {:id "scene"
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes)))

(defn ui-controls []
  [usvg/download-shortcut "scene" "triangle-gradient"])

(sketch/definition triangle-gradient
  {:created-at "2022-01-07"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount (view-sketch/static-page scene :triangle-gradient ui-controls)))
