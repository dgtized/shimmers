(ns shimmers.sketches.dispersion
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn rdirection []
  (tm/* (gv/vec2 (* 0.6 (abs (dr/gaussian 0 3.5)))
                 (* -0.4 (abs (dr/gaussian 0 3.5))))
        (* width 0.09)))

(defn epicenter-distance [epicenter max-dist s]
  (/ (g/dist epicenter (g/centroid s)) max-dist))

(defn possibly-subdivide [p-fn shapes]
  (mapcat (fn [s]
            (if (dr/chance (p-fn s))
              (g/subdivide s)
              [s]))
          shapes))

(defn possibly-disperse [p-fn displacement shapes]
  (map (fn [s]
         (if (dr/chance (p-fn s))
           (geometry/displace s (dr/random) (displacement s))
           s))
       shapes))

(defn shapes []
  (let [building (rect/rect (rv 0.1 0.4) (* width 0.3) (* height 0.6))
        [_ ne _ sw] (g/vertices building)
        max-dist (g/dist ne sw)
        tessellated (g/tessellate building {:num 48})
        divided (possibly-subdivide (fn [s] (- 0.8 (epicenter-distance ne max-dist s))) tessellated)
        shapes (possibly-disperse (fn [s] (- 2.0 (epicenter-distance ne (/ max-dist 2.8) s)))
                                  (fn [_] (rdirection))
                                  divided)]
    (for [shape shapes]
      (vary-meta shape assoc
                 :fill
                 (col/as-css (col/hsla (+ (* 0.005 (dr/gaussian 0 3.5)) 0.55)
                                       (+ (* 0.01 (dr/gaussian 0 3.5)) 0.6)
                                       (-> (epicenter-distance sw width shape)
                                           (tm/clamp 0.25 0.75))
                                       0.9))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "white"
                   :fill "none"
                   :stroke-width 0.02}
    (shapes)))

(sketch/definition dispersion
  {:created-at "2021-03-10"
   :tags #{:static :deterministic}
   :type :quil}
  (ctrl/mount (usvg/page sketch-args scene)))
