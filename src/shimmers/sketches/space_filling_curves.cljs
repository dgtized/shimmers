(ns shimmers.sketches.space-filling-curves
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.svg.core :as svg]))

;; TODO: space-filling-curves can be used to offset map into a texture with
;; interesting locality properties. It can map a 2d coordinate to the closest
;; point on sfc(s) where s is a value from 0 -> 1 representing distance along
;; the curve. Likewise, for the reverse, from length s to a point in 2d space.

(def width 800)
(def height 800)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; https://en.wikipedia.org/wiki/Moore_curve
;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/fu-intr-2_themes-hilbert-sec.html

(defn up-line []
  [:up])

(defn down-line []
  [:down])

(defn left-line []
  [:left])

(defn right-line []
  [:right])

(defn hilbert-curve [n turn]
  (if (zero? n)
    []
    (case turn
      :up
      (concat (hilbert-curve (dec n) :right)
              (up-line)
              (hilbert-curve (dec n) :up)
              (right-line)
              (hilbert-curve (dec n) :up)
              (down-line)
              (hilbert-curve (dec n) :left))
      :left
      (concat (hilbert-curve (dec n) :down)
              (left-line)
              (hilbert-curve (dec n) :left)
              (down-line)
              (hilbert-curve (dec n) :left)
              (right-line)
              (hilbert-curve (dec n) :up))
      :right
      (concat (hilbert-curve (dec n) :up)
              (right-line)
              (hilbert-curve (dec n) :right)
              (up-line)
              (hilbert-curve (dec n) :right)
              (left-line)
              (hilbert-curve (dec n) :down))
      :down
      (concat (hilbert-curve (dec n) :left)
              (down-line)
              (hilbert-curve (dec n) :down)
              (left-line)
              (hilbert-curve (dec n) :down)
              (up-line)
              (hilbert-curve (dec n) :right)))))

(defn turtle [start-pos length curve]
  (reductions (fn [pos dir]
                (case dir
                  :up (tm/+ pos (gv/vec2 0 (- length)))
                  :down (tm/+ pos (gv/vec2 0 length))
                  :right (tm/+ pos (gv/vec2 length 0))
                  :left (tm/+ pos (gv/vec2 (- length) 0))))
              start-pos curve))

(defn turtle->path [positions]
  (svg/path
   (into [[:M (first positions)]]
         (mapv (fn [p] [:L p]) (rest positions)))))

(defn shapes []
  (turtle->path (turtle (rv 0.0 1.0) (/ width (Math/pow 2 7)) (hilbert-curve 7 :up))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (shapes)))

(sketch/definition space-filling-curves
  {:created-at "2022-01-02"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :space-filling-curves)
              "sketch-host"))
