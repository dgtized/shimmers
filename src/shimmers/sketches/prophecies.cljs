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

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

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
                (v/+polar connect (* 0.5 size) (- angle tm/HALF_PI))
                (v/+polar connect size angle)
                (v/+polar connect (* 0.5 size) (+ angle tm/HALF_PI))]))

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
     (v/polar 1 (- (g/heading (tm/- q p)) tm/HALF_PI))]))

(defn stem-face [base height angle]
  (let [connect (v/+polar base height angle)
        rect (rect/rect (tm/+ connect (gv/vec2 (* -0.5 height) (* 1.0 height)))
                        (tm/+ connect (gv/vec2 (* +0.5 height) 0)))]
    (concat [(gl/line2 base connect)
             rect]
            (let [[mid dir] (face-point-out rect 1)]
              (flyout mid (* 0.5 height) (* 0.5 height) (g/heading dir)))
            (let [[mid dir] (face-point-out rect 2)]
              (flyout mid (* 0.5 height) (* 0.5 height) (g/heading dir)))
            (let [[mid dir] (face-point-out rect 3)]
              (flyout mid (* 0.5 height) (* 0.5 height) (g/heading dir))))))

(defn shapes []
  (let [c1 (gc/circle (rv 0.35 0.5) (* width 0.25))
        c2 (gc/circle (rv 0.75 0.5) (* width 0.15))
        meridian (g/scale-size (gl/line2 (:p c1) (:p c2)) 1.75)
        [p q] (g/vertices meridian)
        heading (g/heading (tm/- q p))]
    (concat [c1 c2 meridian]
            (flyout (g/point-at meridian 0.33) (* width 0.1) (* width 0.05) (+ tm/HALF_PI heading))
            (flyout (g/point-at meridian 0.20) (* width -0.1) (* width 0.05) (- tm/HALF_PI heading))
            (stem-face (g/point-at meridian 0.15) (* width 0.03) (+ tm/HALF_PI heading))
            (flyout (g/point-at meridian 0.925) (* width 0.2)
                    (* width 0.1)
                    (+ heading tm/HALF_PI))
            (maybe (partial stem-face (:p c2)
                            (* width (dr/random 0.05 0.1))
                            (+ heading tm/HALF_PI)) 0.5)
            (flyout (g/point-at meridian 0.65) (* width 0.2)
                    (* width 0.1)
                    (- heading tm/HALF_PI)))))

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
