(ns shimmers.sketches.circuit-intersections
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.color.core :as col]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 600)
(def height 800)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn color [i]
  (col/as-css (col/hsla (mod (* i tm/PHI) 1.0) 0.5 0.5 1.0)))

(defn placement [wires size center]
  (let [n (count wires)
        nrange (tm/norm-range (dec n))
        width (+ (* size n) (* (dec n) 0.5 size))
        half (* 0.5 width)]
    (vec (for [offset nrange]
           (+ (- (* offset width) half) center)))))

(defn scene []
  (let [dst-left (mapv #(r 0.1 %) (placement (range 12) 0.01 0.5))
        dst-right-a (mapv #(r 0.9 %) (placement (range 6) 0.01 0.33))
        dst-right-b (mapv #(r 0.9 %) (placement (range 8) 0.01 0.66))
        sources (mapv #(r % 0.9) (placement (range 12) 0.01 0.5))
        destinations (vec (concat dst-left dst-right-a dst-right-b))
        ;; TODO: remove duplicates
        connections (->> (fn [] [(dr/random-int (count sources))
                                (dr/random-int (count destinations))])
                         (repeatedly 12))]
    (csvg/svg {:width width :height height :stroke "black"}
              (for [[i src] (map-indexed vector sources)]
                (svg/line src (gv/vec2 (:x src) (* height 0.1))
                          {:stroke (color i)
                           :key (str "src" i)}))
              (for [[a b] connections
                    :let [p (nth sources a)
                          q (nth destinations b)
                          isec (gv/vec2 (:x p) (:y q))]]
                (svg/group {:key (str (interpose "," [a b]))}
                           (svg/circle isec 4.0 {:key (str "c" isec)})
                           (svg/line isec q {:key (str "l" isec "-" q)}))))))

(defn page []
  [:div (scene)])

(sketch/definition circuit-intersections
  {:created-at "2021-11-08"
   :type :svg
   :tags #{:static}}
  (ctrl/mount page "canvas-host"))
