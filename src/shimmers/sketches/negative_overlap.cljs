(ns shimmers.sketches.negative-overlap
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-rect []
  (let [w (dr/random)
        h (dr/random)
        x (dr/random)
        y (dr/random)]
    (assoc (g/as-polygon (rect/rect (rv x y) (rv w h)))
           :open true)))

(defn good-shape? [poly]
  (let [{[w h] :size} (g/bounds poly)]
    (and (> (g/area poly) 10)
         (> w 20)
         (> h 20))))

(def base-shape
  (assoc (g/as-polygon (rect/rect 0 0 width height)) :open false))

(defn same-shape? [{a :points} {b :points}]
  (every? true? (map tm/delta= a b)))

(comment
  (let [r (random-rect)
        clip (g/clip-with base-shape r)]
    [r clip
     (same-shape? r clip)]))

(defn shapes []
  (->> random-rect
       repeatedly
       (filter good-shape?)
       (take 3)
       (into [base-shape])))

(defn fill-shape [{:keys [open] :as shape}]
  (if open
    shape
    (vary-meta shape assoc :fill "grey")))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (apply list (map fill-shape (shapes)))))

(sketch/definition negative-overlap
  {:created-at "2022-01-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :negative-overlap)
              "sketch-host"))
