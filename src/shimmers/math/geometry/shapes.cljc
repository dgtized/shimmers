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

(defn rectangle-in-bounds
  "Generate a random rectangle inside of an existing bounds."
  [bounds]
  (let [[w h] (repeatedly 2 dr/random)
        p (gv/vec2 (dr/random 0 (- 1 w))
                   (dr/random 0 (- 1 h)))
        q (gv/vec2 w h)]
    (rect/rect (g/unmap-point bounds p)
               (g/unmap-point bounds q))))

(defn square-in-bounds
  [{pos :p [width height] :size}]
  (let [s (dr/random)
        size (if (< height width)
               (* s height)
               (* s width))]
    (rect/rect (tm/+ pos (gv/vec2 (dr/random 0 (- width size))
                                  (dr/random 0 (- height size))))
               size size)))

(comment (rectangle-in-bounds (rect/rect 5 5 5 5))
         (square-in-bounds (rect/rect 5 5 5 5)))
