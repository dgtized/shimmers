(ns shimmers.math.geometry.shapes
  (:require
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; (g/unmap-point (rect/rect 5 5 10 10) (gv/vec2 0.1 0.1))

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
