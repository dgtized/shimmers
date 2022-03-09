(ns shimmers.sketches.delaunator
  (:require
   [delaunator]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]))

(defonce defo (debug/state))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn gen-point []
  (rv (dr/random 0.05 0.95) (dr/random 0.05 0.95)))

;; https://mapbox.github.io/delaunator/
(defn next-half-edge [e]
  (if (= 2 (mod e 3)) (- e 2) (+ e 1)))

(defn prev-half-edge [e]
  (if (= 0 (mod e 3)) (+ e 2) (- e 1)))

(defn triangle-edges [points]
  (let [delaunay (js/Delaunator.from (clj->js points))]
    (for [e (range (.-length (.-triangles delaunay)))
          :when (> e (aget (.-halfedges delaunay) e))
          :let [p (nth points (aget (.-triangles delaunay) e))
                q (nth points (aget (.-triangles delaunay) (next-half-edge e)))]]
      (gl/line2 p q))))

(comment
  (triangle-edges [[0 10] [0 5] [5 5] [4 2]]))

(defn triangles [points]
  (let [delaunay (js/Delaunator.from (clj->js points))]
    (for [t (range (/ (.-length (.-triangles delaunay)) 3))]
      (let [a (aget (.-triangles delaunay) (* 3 t))
            b (aget (.-triangles delaunay) (+ (* 3 t) 1))
            c (aget (.-triangles delaunay) (+ (* 3 t) 2))]
        (gt/triangle2 (nth points a) (nth points b) (nth points c))))))

(comment (triangles [[0 10] [0 5] [5 5] [4 2]]))

(defn circumcenter [[ax ay] [bx by] [cx cy]]
  (let [ad (+ (* ax ax) (* ay ay))
        bd (+ (* bx bx) (* by by))
        cd (+ (* cx cx) (* cy cy))
        D (* 2 (+ (* ax (- by cy))
                  (* bx (- cy ay))
                  (* cx (- ay by))))]
    (gv/vec2 (/ (+ (* ad (- by cy))
                   (* bd (- cy ay))
                   (* cd (- ay cy)))
                D)
             (/ (+ (* ad (- cx bx))
                   (* bd (- ax cx))
                   (* cd (- bx ax)))
                D))))

(defn shapes []
  (let [points (repeatedly 8 gen-point)
        edges (triangle-edges points)
        triangles (triangles points)
        circumcenters (for [{[a b c] :points} triangles]
                        (gc/circle (circumcenter a b c) 3))]
    (swap! defo assoc
           :points points
           :edges triangle-edges
           :circumcenters circumcenters)
    [(svg/group {:fill "black"}
                (for [p points]
                  (gc/circle p 2)))
     #_(svg/group {} edges)
     (svg/group {:fill "none"} triangles)
     (svg/group {:fill "red"} circumcenters)]))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition delaunator
  {:created-at "2022-05-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :delaunator (partial debug/display defo))
              "sketch-host"))
