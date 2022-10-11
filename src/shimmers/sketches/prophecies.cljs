(ns shimmers.sketches.prophecies
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.hexagon :as hex]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; side-effect extend-type to Line2
(require 'shimmers.math.geometry.line)

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

(defn maybe [operation prob]
  (if (dr/chance prob)
    (operation)
    []))

(defn square [connect size angle]
  (-> (rect/rect size)
      (g/center)
      (g/rotate angle)
      (g/translate (tm/+ connect (v/polar (* 0.5 size) angle)))))

(defn point-triangle [connect size angle]
  (gp/polygon2 [connect
                (v/+polar connect size (+ angle 0.5))
                (v/+polar connect size (- angle 0.5))]))

(defn edge-triangle [connect size angle]
  (gp/polygon2 [connect
                (v/+polar connect (* 0.5 size) (left angle))
                (v/+polar connect size angle)
                (v/+polar connect (* 0.5 size) (right angle))]))

(defn flat-hex [connect size angle]
  (-> (v/+polar connect (hex/apothem {:r (* 0.5 size)}) angle)
      (gc/circle (* 0.5 size))
      (hex/flat-hexagon->polygon)))

(defn pointy-hex [connect size angle]
  (-> (v/+polar connect (* 0.5 size) angle)
      (gc/circle (* 0.5 size))
      (hex/pointy-hexagon->polygon)))

(def poly-shapes
  {:square square
   :point point-triangle
   :edge edge-triangle
   :flat-hex flat-hex
   :pointy-hex pointy-hex})

(defn flyout [base height size angle]
  (let [connect (v/+polar base height angle)
        poly ((get poly-shapes (dr/rand-nth (keys poly-shapes)))
              connect size angle)]
    (concat [(gl/line2 base connect)
             poly]
            (maybe (partial deepen poly (dr/random-int 3 8)) 0.5))))

(defn face-point-out [shape face]
  (let [[p q] (nth (g/edges shape) face)]
    [(tm/mix p q 0.5)
     (v/polar 1 (left (g/heading (tm/- q p))))]))

(defn stem-face [base height angle]
  (let [connect (v/+polar base height angle)
        rect (square connect (* 0.5 height) angle)]
    (concat [(gl/line2 base connect)
             rect]
            (mapcat (fn [face]
                      (let [[mid dir] (face-point-out rect face)]
                        (maybe (partial flyout mid (* 0.5 height) (* 0.5 height) (g/heading dir)) 0.5)))
                    (range 4)))))

(defn meridian [c1 c2]
  (let [dir (tm/normalize (tm/- (:p c2) (:p c1)))]
    (gl/line2 (tm/- (:p c1) (tm/* dir (:r c1)))
              (tm/+ (:p c2) (tm/* dir (:r c2))))))

(defn shapes []
  (let [cut (dr/rand-nth [(/ 1 3) (/ 1 4) (/ 2 5)])
        c1-p (rv (dr/random 0.35 0.35) 0.5)
        c2-p (rv (dr/random 0.7 0.75) 0.5)
        d (g/dist c1-p c2-p)
        c1 (gc/circle c1-p (* d (- 1 cut)))
        c2 (gc/circle c2-p (* d cut))
        meridian (meridian c1 c2)
        heading (g/heading meridian)]
    (concat [c1 c2 meridian]
            (flyout (g/point-at meridian 0.10) (* width 0.1) (* width 0.05) (left heading))
            (stem-face (g/point-at meridian 0.2) (* width (dr/random 0.03 0.06)) (right heading))
            (flyout (g/point-at meridian 0.33) (* width 0.1) (* width 0.05) (right heading))
            (flyout (g/point-at meridian 0.925) (* width 0.2)
                    (* width 0.1) (right heading))
            (maybe (partial stem-face (:p c2)
                            (* width (dr/random 0.05 0.1))
                            (right heading)) 0.5)
            (flyout (g/point-at meridian 0.65) (* width 0.2)
                    (* width 0.1)
                    (left heading)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
    (shapes)))

(sketch/definition prophecies
  {:created-at "2022-07-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :prophecies)
              "sketch-host"))
