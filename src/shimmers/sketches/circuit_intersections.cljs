(ns shimmers.sketches.circuit-intersections
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.color.core :as col]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 600)
(def height 800)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn color [i]
  (col/as-css (col/hsla (mod (* i tm/PHI) 1.0) 0.5 0.45 1.0)))

(defn placement [wires size center]
  (let [n (count wires)
        nrange (tm/norm-range (dec n))
        width (+ (* size n) (* (dec n) 0.5 size))
        half (* 0.5 width)]
    (vec (for [offset nrange]
           (+ (- (* offset width) half) center)))))

(defn gen-connections [sources destinations n]
  (let [n-sources (count sources)]
    (loop [i n dests (range (count destinations)) conns []]
      (if (zero? i)
        conns
        (let [d (dr/rand-nth dests)]
          (recur (dec i)
                 (remove #{d} dests)
                 (conj conns [(dr/random-int n-sources) d])))))))

;; Playing with some ideas from https://dl.acm.org/doi/pdf/10.5555/1882723.1882731
;; for VLSI circuit wiring diagrams
(defn scene []
  (let [offset 0.42
        color-base (dr/random)
        dst-left (mapv #(rv 0.1 %) (placement (range 12) 0.01 0.5))
        dst-right-a (mapv #(rv 0.9 %) (placement (range 6) 0.01 0.33))
        dst-right-b (mapv #(rv 0.9 %) (placement (range 8) 0.01 0.66))
        sources (->> [offset 0.5 (- 1.0 offset)]
                     dr/rand-nth
                     (placement (range 12) 0.01)
                     (mapv #(rv % 0.9)))
        destinations (vec (concat dst-left dst-right-a dst-right-b))
        ;; TODO: remove duplicates
        connections (gen-connections sources destinations 12)]
    (csvg/svg-timed {:width width :height height :stroke "black"}
      (concat
       (for [[i src] (map-indexed vector sources)]
         (svg/line src (gv/vec2 (:x src) (* height 0.1))
                   {:stroke (color (+ color-base i))
                    :stroke-width (dr/random 0.5 3.0)}))
       (for [[a b] connections
             :let [p (nth sources a)
                   q (nth destinations b)
                   isec (gv/vec2 (:x p) (:y q))]]
         (csvg/group {}
           (svg/circle isec 2.5)
           (svg/line isec q)))))))

(sketch/definition circuit-intersections
  {:created-at "2021-11-08"
   :type :svg
   :tags #{:static}}
  (ctrl/mount (view-sketch/static-page scene :circuit-intersections)))
