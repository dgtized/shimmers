(ns shimmers.sketches.infographics
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn same-x? [line]
  (let [[a b] (g/vertices line)]
    (= (:x a) (:x b))))

(defn same-y? [line]
  (let [[a b] (g/vertices line)]
    (= (:y a) (:y b))))

(defn duplicate [bounds line]
  (if (dr/chance 0.5)
    [line]
    (let [v (if (same-x? line)
              (gv/vec2 (* (g/width bounds) (dr/random 0.01 0.05)) 0)
              (gv/vec2 0 (* (g/height bounds) (dr/random 0.01 0.05))))]
      [(g/translate line v)
       (g/translate line (tm/- v))])))

(defn lines [{[w h] :size :as bounds} {:keys [p r]}]
  (let [x-dir (dr/chance 0.5)]
    (if x-dir
      (duplicate bounds (gl/line2 (gv/vec2 0 (:y p)) (gv/vec2 w (:y p))))
      (duplicate bounds (gl/line2 (gv/vec2 (:x p) 0) (gv/vec2 (:x p) h))))))

(defn angle-from-quadrant [[rx ry]]
  (* (if (or (and (< rx 0.5) (< ry 0.5))
             (and (> rx 0.5) (> ry 0.5)))
       1 -1)
     (dr/rand-nth [(tm/radians 30) (tm/radians 45) (tm/radians 60)])))

(defn shapes [bounds]
  (let [p (rp/sample-point-inside (g/scale-size bounds 0.75))
        rp (g/map-point bounds p)
        a-circle (gc/circle p (* 0.7 (g/dist p (g/closest-point bounds p))))
        isec-lines (lines bounds a-circle)
        angle (g/heading (first isec-lines))
        line (clip/line-through-point bounds p (angle-from-quadrant rp))
        parallel (clip/line-through-point bounds
                                          (if (tm/delta= angle 0)
                                            (gv/vec2 0
                                                     (if (< (:y p) (* 0.5 height))
                                                       (:y p)
                                                       (- height (:y p))))
                                            (gv/vec2 (if (< (:x p) (* 0.5 width))
                                                       (- width (:x p))
                                                       (:x p))
                                                     0))
                                          angle)
        ]
    (println p angle parallel)
    (concat [(vary-meta a-circle assoc :stroke-width 2.0)]
            isec-lines
            [line parallel])))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (shapes (csvg/screen width height))))

(sketch/definition infographics
  {:created-at "2024-08-05"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
