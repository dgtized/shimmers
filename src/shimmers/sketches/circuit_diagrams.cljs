(ns shimmers.sketches.circuit-diagrams
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.circle :as gc]
   [thi.ng.math.core :as tm]))

;; Concept: tile the plain with regular-n-gons that don't overlap like in
;; regular-tilings then for the tiles with edges connecting to neighboring
;; n-gons, calculate which faces are connectives and use that to inform the
;; random shape generated inside.

(defn random-shape [size]
  (let [n (dr/weighted {3 8 4 4 5 1 6 2 7 1 8 1})
        r (poly/circumradius-side-length n size)]
    (poly/regular-n-gon n r)))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn face-normals [polygon]
  (->>
   polygon
   g/edges
   (map
    (fn [[p q]]
      {:edge [p q] :connected (dr/weighted {false 2 true 1})}))))

(defn draw-face [{:keys [edge connected]}]
  (let [[p q] edge]
    [(vary-meta (gc/circle (tm/mix p q 0.5) 6)
                assoc :fill (if connected "black" "white"))]))

(defn shapes []
  (let [size (* 0.33 height)
        center (rv 0.5 0.5)
        shape (g/center (g/rotate (random-shape size) (dr/random-tau)) center)
        faces (face-normals shape)]
    (into [shape] (mapcat draw-face faces))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))

(sketch/definition circuit-diagrams
  {:created-at "2024-12-10"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
