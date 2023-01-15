(ns shimmers.sketches.brush-strokes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.line :as gl]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.geometry :as geometry]))

(defn generate-brush [line]
  (let [len (tm/mag line)]
    {:line line
     :facing (gv/vec2 1 0)
     :bristles
     (vec
      (for [[a b] (partition 2 (dr/gaussian-range 0.08 0.03 true))]
        (let [c (+ a (* 0.5 (- b a)))]
          (triangle/inscribed-equilateral
           (gc/circle (g/point-at line c) (* len (- b a)))
           (dr/random eq/TAU)))))}))

(defn translate-brush [brush p]
  (-> brush
      (update :line g/translate p)
      (update :bristles (partial mapv (fn [b] (g/translate b p))))))

(defn rotate-brush [{:keys [line] :as brush} t]
  (let [c (g/centroid line)]
    (-> brush
        (update :line geometry/rotate-around-centroid t)
        (update :facing g/rotate t)
        (update :bristles (partial mapv (fn [b] (geometry/rotate-around b c t)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0
   :brush (generate-brush
           (gl/line2 (cq/rel-vec 0.05 0.45)
                     (cq/rel-vec 0.05 0.55)))})

(defn update-state [state]
  (update state :brush translate-brush (gv/vec2 0.5 0.0)))

(defn draw [{:keys [brush]}]
  (q/no-stroke)
  (q/fill 0.0 0.05)
  (doseq [hair (:bristles brush)]
    (cq/draw-polygon hair)))

(sketch/defquil brush-strokes
  :created-at "2023-01-15"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
