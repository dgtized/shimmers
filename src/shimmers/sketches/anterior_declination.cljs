(ns shimmers.sketches.anterior-declination
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
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
      (cond isec
            (conj [(if (g/contains-point? shape p)
                     (gl/line2 isec q)
                     (gl/line2 p isec))]
                  (when-let [isec2 (isec/intersect-line2-edges? (tm/mix isec q 0.00001) q (g/edges shape))]
                    (gl/line2 isec2 q)))
            (and (g/contains-point? shape p) (g/contains-point? shape q))
            []
            :else
            [line]))))

(defn shapes []
  (let [box1 (g/center (g/rotate (rect/rect (* width 0.2)) (dr/random-tau))
                       (tm/+ (rv 0.5 0.5) (dr/randvec2 (* 0.2 width))))
        box2 (g/center (g/rotate (rect/rect (* width 0.2)) (dr/random-tau))
                       (tm/+ (rv 0.5 0.5) (dr/randvec2 (* 0.2 width))))
        box3 (g/center (g/rotate (rect/rect (* width 0.2)) (dr/random-tau))
                       (tm/+ (rv 0.5 0.5) (dr/randvec2 (* 0.2 width))))
        lines (for [t (tm/norm-range 128)]
                (let [y (tm/smoothstep* 0 1 t)]
                  (gl/line2 (rv 0.0 y) (rv 1.0 y))))]
    (->> lines
         (mapcat (fn [line] (cut-line box1 line)))
         (mapcat (fn [line] (cut-line box2 line)))
         (mapcat (fn [line] (cut-line box3 line))))))

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
