(ns shimmers.sketches.inset-grids
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def min-area (* 0.0015 width height))

(defn splits [{[w h] :size}]
  (if (dr/chance 0.33)
    (let [ratio (/ w h)
          n (dr/random-int 1 7)]
      {:rows (math/ceil (/ n ratio))
       :cols (math/ceil (* n ratio))})
    (if (> w h)
      {:rows (dr/random-int 1 5)
       :cols (dr/random-int 2 11)}
      {:rows (dr/random-int 2 11)
       :cols (dr/random-int 1 5)})))

(defn division [limit bounds]
  (let [rect (g/scale-size bounds 0.975)]
    (if (< (g/area rect) limit)
      [rect]
      (mapcat (fn [r]
                (let [r' (if (dr/chance (* 0.3 (- 1.0 (/ (g/area r) (* width height)))))
                           (let [d (dr/rand-nth [0.025 0.0125])]
                             (g/translate
                              r
                              (dr/rand-nth [(v/polar (* d (g/width r)) 0)
                                            (v/polar (* d (g/width r)) (* 0.5 eq/TAU))
                                            (v/polar (* d (g/height r)) (* 0.25 eq/TAU))
                                            (v/polar (* d (g/height r)) (* 0.75 eq/TAU))])))
                           r)]
                  (into [r'] (division limit r'))))
              (g/subdivide rect (splits rect))))))

(defn shapes [bounds]
  (division min-area bounds))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.66}
    (shapes (csvg/screen width height))))

(sketch/definition inset-grids
  {:created-at "2024-07-16"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
