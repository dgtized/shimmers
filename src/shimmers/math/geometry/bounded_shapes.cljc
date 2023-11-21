(ns shimmers.math.geometry.bounded-shapes
  (:require
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.polygon :as poly]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; (g/unmap-point (rect/rect 5 5 10 10) (gv/vec2 0.1 0.1))

;; This seems inherently biased somehow because of the radius fix?
(defn circle
  [{[width height] :size :as bounds}]
  (let [c (gv/vec2 (dr/random) (dr/random))
        [x y] c
        r (* (min (* (min x (- 1 x)) width)
                  (* (min y (- 1 y)) height)))]
    (gc/circle (g/unmap-point bounds c) (dr/random r))))

(comment
  (let [bounds (rect/rect 10 10 20 20)
        circles (repeatedly 1000 #(circle bounds))]
    (every?
     (fn [points] (every? (fn [p] (g/contains-point? bounds p)) points))
     (map (fn [{p :p :as c}] (conj (g/vertices c 12) p)) circles))))

(defn circle-with-radius
  "Construct a random circle in `bounds` with radius `r`.

  `r` is constrained to the minimum side length of the bounds."
  [{[width height] :size :as bounds} r]
  (let [short (min width height)
        r (min r short)
        m (/ r short)
        c (gv/vec2 (dr/random m (- 1 m))
                   (dr/random m (- 1 m)))]
    (gc/circle (g/unmap-point bounds c) r)))

(comment
  (let [bounds (rect/rect 10 10 20 20)
        circles (repeatedly 1000 #(circle-with-radius bounds (dr/random 2 10)))]
    (every?
     (fn [points] (every? (fn [p] (g/contains-point? bounds p)) points))
     (map (fn [{p :p :as c}] (conj (g/vertices c 12) p)) circles))))

(defn max-circle
  "Construct largest circle inside of rectangle `bounds` for a given point."
  [bounds point]
  (let [p (g/unmap-point bounds point)]
    (gc/circle p (poly/dist-to-closest-point bounds p))))

(defn rectangle
  "Generate a random rectangle inside of an existing bounds.

  `w%` and `h%` are percentage sizes [0..1] of the bounding rectangle."
  ([bounds]
   (rectangle bounds (dr/random) (dr/random)))
  ([bounds w% h%]
   (let [p (gv/vec2 (dr/random 0 (- 1 w%))
                    (dr/random 0 (- 1 h%)))
         q (tm/+ p (gv/vec2 w% h%))]
     (rect/rect (g/unmap-point bounds p)
                (g/unmap-point bounds q)))))

(defn square
  "Generates a random square inside the rectangle `bounds`.

  `s%` is the percent size of min(width,height) of the bounding rectangle."
  ([bounds] (square bounds (dr/random)))
  ([{pos :p [width height] :size} s%]
   (let [size (* s% (min height width))]
     (rect/rect (tm/+ pos (gv/vec2 (dr/random 0 (- width size))
                                   (dr/random 0 (- height size))))
                size size))))

(comment (let [r (rectangle (rect/rect 5 5 10 5))]
           [r (:p r) (rect/top-right r)])
         (square (rect/rect 5 5 5 5))
         (square (rect/rect 5 5 5 5) 0.5))
