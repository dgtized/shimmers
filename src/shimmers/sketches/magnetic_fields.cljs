(ns shimmers.sketches.magnetic-fields
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-point-inside [{:keys [p size]}]
  (tm/+ p (dr/random (size 0)) (dr/random (size 1))))

(defn closest [dipoles pos]
  (apply min-key (fn [dipole] (g/dist-squared pos (:p dipole))) dipoles))

(defn line-step [dipoles]
  (fn [pos]
    (let [forces (for [{:keys [p strength]} dipoles
                       :let [[x y] (tm/- pos p)
                             b (tm/cross (gv/vec3 x y 0) (gv/vec3 0 0 strength))
                             a (gv/vec2 (:x b) (:y b))]]
                   (tm/* a (/ 1 (+ (g/dist-squared pos p) 1e-4))))
          force-at-point (reduce tm/+ (gv/vec2) forces)]
      (tm/+ pos (tm/normalize force-at-point 8)))))

(defn split-lines [split-pct points]
  (->> points
       (partition-by (fn [_] (dr/chance split-pct)))
       (filter #(> (count %) 1))
       (mapv gl/linestrip2)))

(defn line [bounds dipoles]
  (let [start (random-point-inside (g/scale-size bounds 0.8))
        split-chance (dr/weighted {1.0 5
                                   0.95 3
                                   0.9 2
                                   0.85 1
                                   0.6 1})]
    (->> start
         (iterate (line-step dipoles))
         (take 100)
         (split-lines split-chance))))

(defn random-dipole [bounds]
  (let [strength (dr/random 2 6)]
    {:p (random-point-inside (g/scale-size bounds 0.5))
     :strength (if (dr/chance 0.5) strength (- strength))}))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        dipoles (repeatedly (dr/random-int 2 5) (partial random-dipole bounds))]
    [(csvg/group {}
       (for [{:keys [p strength]} dipoles]
         (with-meta (gc/circle p (Math/abs strength))
           {:fill (if (> strength 0) "none" "black")})))
     (csvg/group {}
       (apply concat (repeatedly 200 #(line bounds dipoles))))]))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.75}
    (shapes)))

(sketch/definition magnetic-fields
  {:created-at "2022-02-27"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :magnetic-fields)
              "sketch-host"))
