(ns shimmers.sketches.conveyors
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn generate-shape [{[_ y] :p [w h] :size}]
  (let [x-entry (* 0.9 w)
        radius (dr/random (* 0.1 h) (* 0.5 h))
        y-pos (+ y radius (dr/random (- h (* 2 radius))))]
    (gc/circle x-entry y-pos (* radius 0.95))))

(defn convey [bounds dx shape]
  (let [shape' (g/translate shape dx)]
    (when (collide/overlaps? bounds shape')
      shape')))

(defn add-shape [bounds shapes]
  (let [new-shape (generate-shape bounds)]
    (if (or (> (count shapes) 200)
            (some (partial collide/overlaps? new-shape) (take 20 shapes)))
      shapes
      (conj shapes new-shape))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [width (q/width)
        height (q/height)
        bounds (rect/rect 0 (* 0.4 height) width (* 0.2 height))]
    {:t 0
     :velocity (gv/vec2 (- (/ width 200)) 0)
     :bounds bounds
     :shapes []}))

(defn update-state [{:keys [bounds velocity] :as state}]
  (let [dt (dr/random 0.2)]
    (update state :shapes
            (fn [shapes] (->> shapes
                             (keep (partial convey bounds (tm/* velocity dt)))
                             (add-shape bounds))))))

(defn draw [{:keys [shapes bounds]}]
  (q/background 1.0)
  #_(qdg/draw bounds)
  (doseq [s shapes]
    (qdg/draw s)))

(sketch/defquil conveyors
  :created-at "2022-12-31"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
