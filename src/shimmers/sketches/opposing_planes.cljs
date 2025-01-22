(ns shimmers.sketches.opposing-planes
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn ellipse [p a b]
  (gp/polygon2
   (for [t (range 0 eq/TAU (/ eq/TAU 100))]
     (tm/+ p
           (gv/vec2 (* a (math/cos t))
                    (* b (math/sin t)))))))

(defn e-range [r n]
  (range 4.0 r (/ r n)))

(defn shapes []
  (concat
   (for [r (e-range (* height 0.4) 20)]
     (csvg/group {:stroke-width (tm/clamp (dr/gaussian 1.9 0.8) 0.5 10.0)}
       (ellipse (tm/+ (rv 0.5 0.5) (dr/jitter (/ r 12.0)))
                (* 1.3 r) (* 0.8 r))))
   (for [r (dr/gaussian-range (* height (/ 0.4 25)) (* height (/ 0.4 50))
                              true [4.0 (* height 0.45)])]
     (csvg/group {:stroke-width (tm/clamp (dr/gaussian 2.9 1.5) 0.5 10.0)}
       (ellipse (tm/+ (rv 0.5 0.5) (dr/jitter (/ r 12.0)))
                (* 1.4 r) (* 1.75 r))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes)))

(sketch/definition opposing-planes
  {:created-at "2025-01-19"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
