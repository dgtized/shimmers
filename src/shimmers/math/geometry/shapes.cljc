(ns shimmers.math.geometry.shapes
  (:require
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; (g/unmap-point (rect/rect 5 5 10 10) (gv/vec2 0.1 0.1))

;; This seems inherently biased somehow because of the radius fix?
(defn circle-in-bounds
  [{[width height] :size :as bounds}]
  (let [c (gv/vec2 (dr/random) (dr/random))
        [x y] c
        r (* (min (* (min x (- 1 x)) width)
                  (* (min y (- 1 y)) height)))]
    (gc/circle (g/unmap-point bounds c) (dr/random r))))

(comment
  (let [bounds (rect/rect 10 10 20 20)
        circles (repeatedly 1000 #(circle-in-bounds bounds))]
    (every?
     (fn [points] (every? (fn [p] (g/contains-point? bounds p)) points))
     (map (fn [{p :p :as c}] (conj (g/vertices c 12) p)) circles))))

(defn circle-in-bounds-with-radius
  [{[width height] :size :as bounds} r0 r1]
  (let [r (dr/random r0 r1)
        m (/ r (min width height))
        c (gv/vec2 (dr/random m (- 1 m))
                   (dr/random m (- 1 m)))]
    (gc/circle (g/unmap-point bounds c) r)))

(comment
  (let [bounds (rect/rect 10 10 20 20)
        circles (repeatedly 1000 #(circle-in-bounds-with-radius bounds 2 10))]
    (every?
     (fn [points] (every? (fn [p] (g/contains-point? bounds p)) points))
     (map (fn [{p :p :as c}] (conj (g/vertices c 12) p)) circles))))

(defn rectangle-in-bounds
  "Generate a random rectangle inside of an existing bounds.

  `w%` and `h%` are percentage sizes [0..1] of the bounding rectangle."
  ([bounds]
   (rectangle-in-bounds bounds (dr/random) (dr/random)))
  ([bounds w% h%]
   (let [p (gv/vec2 (dr/random 0 (- 1 w%))
                    (dr/random 0 (- 1 h%)))
         q (tm/+ p (gv/vec2 w% h%))]
     (rect/rect (g/unmap-point bounds p)
                (g/unmap-point bounds q)))))

(defn square-in-bounds
  "Generates a random square inside the rectangle `bounds`.

  `s%` is the percent size of min(width,height) of the bounding rectangle."
  ([bounds] (square-in-bounds bounds (dr/random)))
  ([{pos :p [width height] :size} s%]
   (let [size (* s% (min height width))]
     (rect/rect (tm/+ pos (gv/vec2 (dr/random 0 (- width size))
                                   (dr/random 0 (- height size))))
                size size))))

(comment (let [r (rectangle-in-bounds (rect/rect 5 5 10 5))]
           [r (:p r) (rect/top-right r)])
         (square-in-bounds (rect/rect 5 5 5 5))
         (square-in-bounds (rect/rect 5 5 5 5) 0.5))
