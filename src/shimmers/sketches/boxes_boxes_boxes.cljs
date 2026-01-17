(ns shimmers.sketches.boxes-boxes-boxes
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
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

(defn size-factor []
  (dr/rand-nth [0.33 0.5 0.66 0.75 0.8 0.9 1.0 1.1 1.2 1.33 1.5]))

(defn offset-factor []
  (dr/random -0.8 0.8))

(defn adjacent-box [{:keys [p size] :as parent} gap]
  (let [[w h] size
        wsize (* (size-factor) w)
        hsize (* (size-factor) h)
        woff (* (offset-factor) wsize)
        hoff (* (offset-factor) hsize)
        direction (dr/rand-nth [:north :south :east :west])]
    (-> (case direction
          :north (rect/rect (gv/vec2 (+ woff) (- (+ h gap)))
                            (gv/vec2 (+ wsize woff) (- gap)))
          :east (rect/rect (gv/vec2 (+ w gap) (+ hoff))
                           (gv/vec2 (+ (* w 2) gap) (+ hoff hsize)))
          :south (rect/rect (gv/vec2 (+ woff) (+ h gap))
                            (gv/vec2 (+ wsize woff) (+ (* h 2) gap)))
          :west (rect/rect (gv/vec2 (- (+ w gap)) (+ hoff))
                           (gv/vec2 (- gap) (+ hoff hsize))))
        (g/translate p)
        (annotate parent direction))))

(defn satisfying? [bounds gap boxes]
  (fn [box]
    (let [scale-box (poly-detect/inset-polygon (g/as-polygon box) (+ 1 (- gap)))]
      (when (and (collide/bounded? bounds box)
                 (not-any? (fn [x] (collide/overlaps? scale-box x)) boxes)
                 (> (g/width box) (* 0.02 (g/width bounds)))
                 (> (g/height box) (* 0.02 (g/height bounds))))
        box))))

(defn add-box [bounds boxes]
  (let [gap 4.0
        generate (fn [] (adjacent-box (dr/rand-nth boxes) gap))
        box (some (satisfying? bounds gap boxes) (repeatedly 8 generate))]
    (if (some? box)
      (conj boxes box)
      boxes)))

(defn add-hatching [box parent]
  (let [rotation (dr/weighted {0.0 3.0
                               0.01 1.0
                               0.02 1.0})]
    (csvg/group {:stroke-width 0.25}
      (for [line (clip/hatch-rectangle
                  box (* (dr/random 0.075 0.18)
                         (min (g/width box) (g/height box)))
                  (+ (g/heading (tm/- (g/centroid parent) (g/centroid box)))
                     (* eq/TAU 0.25)))]
        (-> line
            (g/scale-size (dr/random 0.66 1.0))
            (geometry/rotate-around-centroid (dr/gaussian 0.0 rotation)))))))

(defn nesting [box]
  (let [spacings (dr/weighted [[(dr/gaussian-range 0.15 0.02) 1.0]
                               [(tm/norm-range (dr/random-int 5 10)) 1.0]])
        rotation (dr/weighted {0.0 3.0
                               0.01 1.0
                               0.02 1.0})]
    (for [offset spacings]
      (let [i (* offset (/ (geometry/min-axis box) 2.1))]
        (-> box
            g/as-polygon
            (geometry/rotate-around-centroid (dr/gaussian 0.0 rotation))
            (poly-detect/inset-polygon i))))))

(defn shapes [bounds]
  (let [start (g/scale-size bounds (dr/random 0.08 0.15))]
    (for [box (nth (iterate (partial add-box bounds) [start])
                   (dr/random-int 24 64))]
      (let [{:keys [parent]} (meta box)]
        (csvg/group {}
          (conj [(vary-meta box dissoc :parent)]
                (if (and parent (dr/chance 0.9))
                  (add-hatching box parent)
                  (nesting box))))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.75}
    (csvg/group {}
      (shapes (csvg/screen width height)))
    (when (dr/chance 0.33)
      (csvg/group {:fill "white"}
        (shapes (csvg/screen width height))))))

(defn explanation [_]
  [:div
   [:p "Genuary 2026 - Day 12 - Boxes Only"]])

(sketch/definition boxes-boxes-boxes
  {:created-at "2026-01-13"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))
