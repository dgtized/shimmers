(ns shimmers.sketches.anterior-declination
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn cut-line [shape line]
  (when line
    (let [[p q] (g/vertices line)
          isec (isec/intersect-line2-edges? p q (g/edges shape))]
      (cond (and (g/contains-point? shape p) (g/contains-point? shape q))
            []
            isec
            (conj [(if (g/contains-point? shape p)
                     (gl/line2 isec q)
                     (gl/line2 p isec))]
                  (when-let [isec2 (isec/intersect-line2-edges? (tm/mix isec q 0.00001) q (g/edges shape))]
                    (gl/line2 isec2 q)))
            :else
            [line]))))

(defn add-shape [shapes]
  (let [shape (poly/regular-n-gon (dr/random-int 3 7) (* width (dr/random 0.066 0.225)))
        candidate (g/center (g/rotate shape (dr/random-tau))
                            (tm/+ (rv 0.5 0.5) (dr/randvec2 (* 0.2 width))))]
    (if (and (collide/bounded? (g/scale-size (csvg/screen width height) 0.975) candidate)
             (not-any? (fn [s] (when (collide/overlaps? candidate s)
                                s))
                       shapes))
      (conj shapes candidate)
      shapes)))

(defn shapes []
  (let [shapes (nth (iterate add-shape []) (dr/random-int 4 12))
        lines (for [t ((dr/weighted [[(fn [] (tm/norm-range 128)) 1.0]
                                     [(fn [] (dr/gaussian-range (/ 1.0 128) (/ 1.0 256))) 1.0]]))]
                (let [y (tm/smoothstep* 0 1 t)]
                  (gl/line2 (rv 0.0 y) (rv 1.0 y))))]
    (reduce (fn [lines shape] (mapcat (fn [line] (cut-line shape line)) lines))
            lines shapes)))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))

(sketch/definition anterior-declination
  {:created-at "2025-01-01"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
