(ns shimmers.sketches.displacements-inbetween
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-line [a b controls scale]
  (let [perpendicular (tm/normalize (g/normal (tm/- b a)) scale)]
    (-> (concat [a]
                (for [t (cs/midsection (tm/norm-range (inc controls)))]
                  (tm/+ (tm/mix a b t)
                        (tm/* perpendicular (dr/random -1 1))))
                [b])
        bezier/auto-spline2
        (g/sample-uniform 8.0 true)
        gl/linestrip2)))

(defn mix-line [path-a path-b factor]
  (gl/linestrip2
   (let [samples (max (count (:points path-a))
                      (count (:points path-b)))]
     (for [t (tm/norm-range (dec samples))]
       (tm/mix (g/point-at path-a t) (g/point-at path-b t) factor)))))

(defn base-lines []
  (let [a (-> (make-line (r 0.1 0.1) (r 0.1 0.9) 2 (* 0.08 width))
              (g/rotate (dr/random -0.05 0.1))
              (assoc :stroke-width 2.0))
        b (-> (make-line (r 0.5 0.0) (r 0.5 1.0) 3 (* 0.12 width))
              (g/rotate (dr/random -0.05 0.05))
              (assoc :stroke-width 2.0))
        c (-> (make-line (r 0.9 0.1) (r 0.9 0.9) 2 (* 0.08 width))
              (g/rotate (if (dr/chance 0.1)
                          (dr/random -0.8 0.8)
                          (dr/random 0.05 -0.1)))
              (assoc :stroke-width 2.0))]
    (concat [a]
            (for [t (tm/norm-range 11)]
              (mix-line a b t))
            [b]
            (for [t (tm/norm-range 17)]
              (mix-line b c t))
            [c])))

(defn connect [[a b] t]
  (gl/linestrip2 (g/point-at a t) (g/point-at b t)))

(defn lines []
  (let [lines (base-lines)
        pairs (dr/random-sample 0.3 (partition 2 1 lines))]
    (concat lines
            (repeatedly 64 (fn [] (connect (dr/rand-nth pairs) (dr/random)))))))

(def copy-attribs [:stroke :fill :stroke-width])
(def screen (g/scale (rect/rect 0 0 width height) 0.99))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :stroke-width 1.0}
            (for [[i line] (->> (lines)
                                (gu/fit-all-into-bounds screen)
                                (map-indexed vector))]
              (svg/polyline (:points line)
                            (merge {:key (str "l" i)}
                                   (select-keys line copy-attribs))))))

(defn page []
  [:div (scene)])

(sketch/definition displacements-inbetween
  {:created-at "2021-11-13"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page "canvas-host"))
