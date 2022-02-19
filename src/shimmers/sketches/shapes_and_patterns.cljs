(ns shimmers.sketches.shapes-and-patterns
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn circle-row [v row-height]
  (let [diameter row-height
        cols (tm/floor (/ width diameter))
        r (* 0.475 diameter)]
    (for [u (tm/norm-range cols)]
      (gc/circle (rv (+ u (/ r width)) (+ v (/ r height))) r))))

(defn triangle-row [v row-height]
  (let [base row-height
        cols (tm/floor (/ width base))
        up (dr/chance 0.5)
        triangle (-> (gt/equilateral2 (gv/vec2 0.0 0.0)
                                      (gv/vec2 (* -0.8 base) 0.0))
                     g/center
                     (g/rotate (if up 0 (* 0.5 eq/TAU))))]
    (for [u (tm/norm-range cols)]
      (g/translate triangle (rv (+ u (* 0.5 (/ base width)))
                                (+ v (* 0.5 (/ base height))))))))

;; fixme, align the top/bottom of each triangle?
(defn updown-row [v row-height]
  (let [base row-height
        cols (tm/floor (/ width (* 0.8 base)))
        triangle (-> (gt/equilateral2 (gv/vec2 0.0 0.0)
                                      (gv/vec2 (* -0.8 base) 0.0))
                     g/center)
        freq (dr/random-int 2 5)]
    (for [[idx u] (map-indexed vector (tm/norm-range cols))]
      (-> triangle
          (g/rotate (if (zero? (mod idx freq)) (* 0.5 eq/TAU) 0))
          (g/translate (rv (+ u (* 0.5 (/ base width)))
                           (+ v (* 0.5 (/ base height)))))))))

(defn shapes [rows]
  (let [ranges (dr/var-range rows)
        heights (map - (rest ranges) ranges)]
    (for [[v gap] (map vector ranges heights)
          :let [row-height (* gap height)]]
      ((dr/weighted {#(circle-row v row-height) 1
                     #(triangle-row v row-height) 1
                     #(updown-row v row-height) 1})))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0}
            (apply list (shapes 17))))

(sketch/definition shapes-and-patterns
  {:created-at "2022-02-19"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :shapes-and-patterns)
              "sketch-host"))
