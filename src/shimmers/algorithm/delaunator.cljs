(ns shimmers.algorithm.delaunator
  (:require
   ["delaunator"]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]))

;; Most of the following functions are translated into ClojureScript from
;; https://mapbox.github.io/delaunator/ and then converted into thi.ng/geom type
;; records.
(defn next-half-edge [e]
  (if (= 2 (mod e 3)) (- e 2) (+ e 1)))

(defn prev-half-edge [e]
  (if (= 0 (mod e 3)) (+ e 2) (- e 1)))

(defn delaunator-from ^js/Delaunator [points]
  (js/Delaunator.from (clj->js points)))

(defn triangle-edges [points]
  (let [^js/Delaunator delaunay (delaunator-from points)
        triangles (.-triangles delaunay)
        half-edges (.-halfedges delaunay)]
    (for [e (range (alength triangles))
          :when (> e (aget half-edges e))
          :let [p (nth points (aget triangles e))
                q (nth points (aget triangles (next-half-edge e)))]]
      [p q])))

(comment
  (triangle-edges [[0 10] [0 5] [5 5] [4 2]]))

(defn delaunay-triangle [points ^js/Delaunator delaunay t]
  (let [triangles (.-triangles delaunay)
        a (aget triangles (* 3 t))
        b (aget triangles (+ (* 3 t) 1))
        c (aget triangles (+ (* 3 t) 2))]
    (gt/triangle2 (nth points a) (nth points b) (nth points c))))

(defn triangles [points]
  (let [^js/Delaunator delaunay (delaunator-from points)
        triangles (.-triangles delaunay)]
    (for [t (range (/ (alength triangles) 3))]
      (delaunay-triangle points delaunay t))))

(comment (triangles [[0 10] [0 5] [5 5] [4 2]]))

;; Something about this is wrong, but not clear what, so using gt/circumcircle
(defn circumcenter [[ax ay] [bx by] [cx cy]]
  (let [ad (+ (* ax ax) (* ay ay))
        bd (+ (* bx bx) (* by by))
        cd (+ (* cx cx) (* cy cy))
        D (* 2 (+ (* ax (- by cy))
                  (* bx (- cy ay))
                  (* cx (- ay by))))]
    (gv/vec2 (* (/ 1 D)
                (+ (* ad (- by cy))
                   (* bd (- cy ay))
                   (* cd (- ay cy))))
             (* (/ 1 D)
                (+ (* ad (- cx bx))
                   (* bd (- ax cx))
                   (* cd (- bx ax)))))))

(defn triangle-of-edge [e]
  (Math/floor (/ e 3)))

(defn triangle-center [points ^js/Delaunator delaunay t]
  (let [{[a b c] :points} (delaunay-triangle points delaunay t)]
    (first (gt/circumcircle-raw a b c))))

(defn voronoi-edges [points]
  (let [^js/Delaunator delaunay (delaunator-from points)]
    (for [e (range (alength (.-triangles delaunay)))
          :when (< e (aget (.-halfedges delaunay) e))]
      [(triangle-center points delaunay
                        (triangle-of-edge e))
       (triangle-center points delaunay
                        (triangle-of-edge (aget (.-halfedges delaunay) e)))])))

(defn edges-around-point [^js/Delaunator delaunay start]
  (loop [incoming start result []]
    (let [r (conj result incoming)
          outgoing (next-half-edge incoming)
          incoming (aget (.-halfedges delaunay) outgoing)]
      (if (and (not= incoming -1) (not= incoming start))
        (recur incoming r)
        r))))

(defn voronoi-polygons [points]
  (let [^js/Delaunator delaunay (delaunator-from points)
        triangles (.-triangles delaunay)]
    (loop [e 0 seen #{} out []]
      (if (< e (alength triangles))
        (let [p (aget triangles (next-half-edge e))]
          (if (contains? seen p)
            (recur (inc e) seen out)
            (let [edges (edges-around-point delaunay e)
                  vertices (for [t (map triangle-of-edge edges)]
                             (triangle-center points delaunay t))]
              (recur (inc e) (conj seen p)
                     (conj out (gp/polygon2 vertices))))))
        out))))

(comment (voronoi-polygons [[0 0] [10 0] [10 10] [0 10] [5 5]]))


