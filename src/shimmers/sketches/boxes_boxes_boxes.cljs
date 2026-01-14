(ns shimmers.sketches.boxes-boxes-boxes
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn annotate [box parent direction]
  (vary-meta box assoc
             :parent parent
             :direction direction))

(defn adjacent-box [{:keys [p size] :as parent}]
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
     parent
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
  (let [start (g/scale-size bounds (dr/random 0.08 0.15))]
    (for [box (nth (iterate (partial add-box bounds) [start])
                   (dr/random-int 24 64))]
      (let [{:keys [parent]} (meta box)]
        (csvg/group {}
          (conj [(vary-meta box dissoc :parent)]
                (when parent
                  (println parent)
                  (csvg/group {:stroke-weight 0.5}
                    (for [line (clip/hatch-rectangle
                                box (* (dr/random 0.075 0.25)
                                       (min (g/width box) (g/height box)))
                                (+ (g/heading (tm/- (g/centroid parent) (g/centroid box)))
                                   (* eq/TAU 0.25)))]
                      (g/scale-size line (dr/random 0.66 1.0)))))))))))

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
