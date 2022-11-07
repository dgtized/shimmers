(ns shimmers.sketches.prophecies
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   ;; side-effect extend-type to Line2
   [shimmers.math.geometry.line]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.hexagon :as hex]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn right [angle]
  (+ angle tm/HALF_PI))

(defn left [angle]
  (- angle tm/HALF_PI))

(defn deepen [polygon n]
  (let [scale (/ 0.8 n)]
    (->> polygon
         (iterate (fn [poly]
                    (g/scale-size poly (- 1.0 scale))))
         (take (inc n)))))

(defn nested [polygon n]
  (->> (range 0.0 0.8 (/ 0.8 n))
       (mapv (fn [s] (g/scale-size polygon (- 1.0 s))))))

(defn maybe [operation prob]
  (if (dr/chance prob)
    (operation)
    []))

(defn square [connect size angle]
  (-> (rect/rect size)
      (g/center)
      (g/rotate angle)
      (g/translate (tm/+ connect (v/polar (* 0.5 size) angle)))))

(defn circle [connect size angle]
  (-> connect
      (v/+polar (* 0.5 size) angle)
      (gc/circle (* 0.5 size))))

(defn point-triangle [connect size angle]
  (gp/polygon2 [connect
                (v/+polar connect size (- angle 0.5))
                (v/+polar connect size (+ angle 0.5))]))

(defn edge-triangle [connect size angle]
  (gp/polygon2 [(v/+polar connect (* 0.5 size) (left angle))
                (v/+polar connect size angle)
                (v/+polar connect (* 0.5 size) (right angle))]))

(defn flat-hex [connect size angle]
  (let [r (* 0.5 size)
        center (v/+polar connect (hex/apothem {:r r}) angle)]
    (-> (gc/circle r)
        (hex/flat-hexagon->polygon)
        (g/rotate (+ angle (/ Math/PI 6)))
        (g/translate center))))

(defn pointy-hex [connect size angle]
  (let [r (* 0.5 size)
        center (v/+polar connect r angle)]
    (-> (gc/circle r)
        (hex/pointy-hexagon->polygon)
        (g/rotate (+ angle (/ Math/PI 6)))
        (g/translate center))))

;; https://en.wikipedia.org/wiki/Pentagon
(defn flat-pentagon [connect size angle]
  (let [size (* 0.66 size)
        r (/ 1 (* 2 (Math/sqrt (- 5 (Math/sqrt 20)))))
        R (Math/sqrt (/ (+ 5 (Math/sqrt 5)) 10))
        center (v/+polar connect (* r size) angle)
        r72 (tm/radians 72)]
    (gp/polygon2 (v/+polar center (* R size) (+ angle (* r72 0)))
                 (v/+polar center (* R size) (+ angle (* r72 1)))
                 (v/+polar center (* R size) (+ angle (* r72 2)))
                 (v/+polar center (* R size) (+ angle (* r72 3)))
                 (v/+polar center (* R size) (+ angle (* r72 4))))))

(defn pointy-pentagon [connect size angle]
  (let [size (* 0.66 size)
        R (Math/sqrt (/ (+ 5 (Math/sqrt 5)) 10))
        center (v/+polar connect (* R size) angle)
        r72 (tm/radians 72)
        angle (- angle (* 0.5 r72))]
    (gp/polygon2 (v/+polar center (* R size) (+ angle (* r72 0)))
                 (v/+polar center (* R size) (+ angle (* r72 1)))
                 (v/+polar center (* R size) (+ angle (* r72 2)))
                 (v/+polar center (* R size) (+ angle (* r72 3)))
                 (v/+polar center (* R size) (+ angle (* r72 4))))))

(def poly-shapes
  {:square square
   :circle circle
   :point point-triangle
   :edge edge-triangle
   :flat-pentagon flat-pentagon
   :pointy-pentagon pointy-pentagon
   :flat-hex flat-hex
   :pointy-hex pointy-hex})

(defn flyout [base height size angle]
  (let [connect (v/+polar base height angle)
        poly ((get poly-shapes (dr/rand-nth (keys poly-shapes)))
              connect size angle)
        operator (dr/rand-nth [deepen nested])]
    (concat [(gl/line2 base connect)
             poly]
            (maybe (partial operator poly (dr/random-int 3 8)) 0.5))))

(defn point-on-segment? [point p q]
  (< (g/dist-squared point (gu/closest-point-on-segment point p q)) 1))

(defn face-point-out [[p q]]
  [(tm/mix p q 0.5)
   (v/polar 1 (left (g/heading (tm/- q p))))])

(defn stem-face [base height angle]
  (let [connect (v/+polar base height angle)
        shape ((dr/rand-nth (vals (dissoc poly-shapes :circle)))
               connect (* 0.5 height) angle)]
    (concat [(gl/line2 base connect)
             shape]
            (->> (g/edges shape)
                 (remove (fn [[p q]] (point-on-segment? connect p q)))
                 (mapcat (fn [face]
                           (let [[mid dir] (face-point-out face)]
                             (maybe (partial flyout mid (* 0.5 height) (* 0.5 height) (g/heading dir)) 0.5))))))))

(defn meridian [c1 c2]
  (let [dir (tm/normalize (tm/- (:p c2) (:p c1)))]
    (gl/line2 (tm/- (:p c1) (tm/* dir (:r c1)))
              (tm/+ (:p c2) (tm/* dir (:r c2))))))

(defn make-shape [vertex heading shapes]
  (let [direction ((dr/rand-nth [right left]) heading)
        new-shapes ((dr/weighted {#(flyout vertex (* width 0.1) (* width 0.05) direction) 3
                                  #(flyout vertex (* width 0.1) (* width 0.1) direction) 1}))]
    (when (and (not-any? (fn [s] (collide/overlaps? s (first new-shapes))) shapes)
               (not-any? (fn [s] (collide/overlaps? s (second new-shapes))) shapes))
      new-shapes)))

(defn add-shapes [shapes vertices heading n]
  (loop [n n vertices vertices shapes shapes]
    (if (zero? n)
      [shapes vertices]
      (if-let [new-shapes (make-shape (first vertices) heading shapes)]
        (recur (dec n) (rest vertices) (concat shapes new-shapes))
        (recur n vertices shapes)))))

(defn shapes []
  (let [cut (dr/rand-nth [(/ 1 3) (/ 1 4) (/ 2 5)])
        c1-p (rv (dr/random 0.35 0.35) 0.5)
        c2-p (rv (dr/random 0.7 0.75) 0.5)
        d (g/dist c1-p c2-p)
        c1 (gc/circle c1-p (* d (- 1 cut)))
        c2 (gc/circle c2-p (* d cut))
        meridian (meridian c1 c2)
        heading (g/heading meridian)
        vertices (->> (dr/random-int 8 16)
                      (g/vertices meridian)
                      (drop-last 1)
                      (drop 1)
                      dr/shuffle)
        stems (concat (stem-face (nth vertices 0) (* width (dr/random 0.03 0.06)) ((dr/rand-nth [left right]) heading))
                      (maybe (partial stem-face (nth vertices 1)
                                      (* width (dr/random 0.05 0.1))
                                      ((dr/rand-nth [left right]) heading)) 0.5))
        [shapes _] (add-shapes stems (drop 2 vertices) heading 4)]
    (concat [c1 c2 meridian]
            shapes)))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0}
    (shapes)))

(sketch/definition prophecies
  {:created-at "2022-07-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :prophecies)
              "sketch-host"))
