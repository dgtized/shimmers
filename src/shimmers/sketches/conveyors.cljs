(ns shimmers.sketches.conveyors
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn square [circle angle]
  (geometry/rotate-around-centroid
   (g/bounds (g/scale-size circle 0.4))
   angle))

(defn rod [{:keys [p r]} angle]
  (gp/polygon2 (v/+polar p r (- angle (/ eq/TAU 3)))
               (v/+polar p r (- angle (/ eq/TAU 6)))
               (v/+polar p r (+ angle (/ eq/TAU 6)))
               (v/+polar p r (+ angle (/ eq/TAU 3)))))

(defn generate-shape [{[_ y] :p [w h] :size}]
  (let [x-entry (* w 0.99)
        radius (dr/random (* 0.2 h) (* 0.5 h))
        y-pos (+ y radius (dr/random (- h (* 2 radius))))
        circle (gc/circle x-entry y-pos (* radius 0.95))
        h-circle (g/scale-size circle 0.5)]
    ((dr/weighted
      {(fn [] circle) 1
       (fn [] h-circle) 4
       (fn [] (square circle (dr/random-tau))) 1
       (fn [] (gp/polygon2
              (take 3 (:points (square h-circle (dr/random-tau)))))) 0.5
       (fn [] (triangle/inscribed-equilateral h-circle (dr/random-tau))) 2
       (fn [] (rod h-circle (dr/random-tau))) 1
       (fn [] (-> (g/scale-size circle 0.75)
                 (g/as-polygon (dr/rand-nth [5 6 7 8]))
                 (geometry/rotate-around-centroid (dr/random-tau)))) 6}))))

(defn convey [bounds dx shape]
  (let [shape' (g/translate shape dx)]
    (when (collide/overlaps? bounds shape')
      shape')))

(defn add-shape [bounds shapes]
  (let [new-shape (generate-shape bounds)]
    (if (or (> (count shapes) 500)
            (some (partial collide/overlaps? (g/scale-size new-shape 1.25))
                  (take 20 shapes)))
      shapes
      (conj shapes new-shape))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [width (q/width)
        height (q/height)
        bounds (rect/rect 0 (/ height 3) width (/ height 3))]
    {:t 0
     :velocity (gv/vec2 (- (/ width 200)) 0)
     :bounds bounds
     :shapes []}))

(defn update-state [{:keys [bounds velocity] :as state}]
  (let [dt (dr/random 0.2)]
    (update state :shapes
            (fn [shapes] (->> shapes
                             (keep (partial convey bounds (tm/* velocity dt)))
                             (add-shape bounds)
                             (add-shape bounds)
                             (add-shape bounds))))))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (doseq [s shapes]
    (qdg/draw s)))

(defn page []
  (sketch/component
   :size [900 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition conveyors
  {:created-at "2022-12-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
