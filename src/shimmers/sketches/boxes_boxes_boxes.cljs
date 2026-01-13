(ns shimmers.sketches.boxes-boxes-boxes
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.core :as g]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]
   [shimmers.math.geometry.collisions :as collide]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn annotate [box direction]
  (vary-meta box assoc :fill (case direction
                               :north "red"
                               :east "green"
                               :south "blue"
                               :west "yellow")))

(defn adjacent-box [{:keys [p size]}]
  (let [[w h] size
        gap 4.0
        wsize (* (dr/rand-nth [0.33 0.5 0.66 0.75 0.8 0.9 1.0 1.1 1.2 1.33 1.5]) w)
        hsize (* (dr/rand-nth [0.33 0.5 0.66 0.75 0.8 0.9 1.0 1.1 1.2 1.33 1.5]) h)
        woff (* (dr/random -0.8 0.8) wsize)
        hoff (* (dr/random -0.8 0.8) hsize)
        direction (dr/rand-nth [:north :south :east :west])]
    (annotate
     (case direction
       :north (rect/rect (tm/+ p (gv/vec2 (+ woff) (- (+ h gap))))
                         (tm/+ p (gv/vec2 (+ wsize woff) (- gap))))
       :east (rect/rect (tm/+ p (gv/vec2 (+ w gap) (+ hoff)))
                        (tm/+ p (gv/vec2 (+ (* w 2) gap) (+ hoff hsize))))
       :south (rect/rect (tm/+ p (gv/vec2 (+ woff) (+ h gap)))
                         (tm/+ p (gv/vec2 (+ wsize woff) (+ (* h 2) gap))))
       :west (rect/rect (tm/+ p (gv/vec2 (- (+ w gap)) (+ hoff)))
                        (tm/+ p (gv/vec2 (- gap) (+ hoff hsize)))))
     direction)))

(defn satisfying? [bounds boxes]
  (fn [box]
    (when (and (collide/bounded? bounds box)
               (not-any? (fn [x] (collide/overlaps? box x)) boxes)
               (> (g/width box) (* 0.02 (g/width bounds)))
               (> (g/height box) (* 0.02 (g/height bounds))))
      box)))

(defn add-box [bounds boxes]
  (let [generate (fn [] (adjacent-box (dr/rand-nth boxes)))
        box (some (satisfying? bounds boxes) (repeatedly 32 generate))]
    (if (some? box)
      (conj boxes box)
      boxes)))

(defn shapes [bounds]
  (let [start (g/scale-size bounds (dr/random 0.06 0.12))]
    (nth (iterate (partial add-box bounds) [start]) 16)))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes (csvg/screen width height))))

(sketch/definition boxes-boxes-boxes
  {:created-at "2026-01-13"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
