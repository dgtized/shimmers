(ns shimmers.sketches.skyline
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn building [w h]
  [(rect/rect 0 0 (* width w) (* height h))])

(defn shapes []
  (apply concat
         (for [[a b] (->> (dr/density-range 0.025 0.05 false)
                          (filter (fn [x] (<= 0.05 x 0.95)))
                          (partition 2 1))
               :let [h (dr/gaussian (- 0.4 (abs (- b 0.5))) 0.025)]]
           (concat (mapv (fn [s] (g/translate s (tm/+ (rv 0.0 0.6) (gv/vec2 (* width a) (* height (- h))))))
                         (building (- b a) h))
                   (mapv (fn [s] (g/translate
                                 s
                                 (tm/+ (rv 0.0 0.6) (gv/vec2 (* width a) 0.0))))
                         (building (- b a) (* 0.7 h)))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))


(defn explanation [_]
  [:p "Genuary 2025 Day 20 - Generative Architecture"])


(sketch/definition skyline
  {:created-at "2025-01-20"
   :tags #{:genuary2025}
   :type :svg}
  (ctrl/mount
   (usvg/page (assoc sketch-args
                     :explanation explanation)
              scene)))
