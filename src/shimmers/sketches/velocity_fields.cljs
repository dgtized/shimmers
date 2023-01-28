(ns shimmers.sketches.velocity-fields
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.core :as g]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-path [bounds seed scale lifespan]
  (fn []
    (let [start (rv (dr/random -0.1 1.1)
                    (dr/random -0.1 1.1))
          path
          (->> [start (gv/vec2)]
               (iterate
                (fn [[p v]]
                  (let [noise (dr/noise-at-point-01 seed scale p)
                        v' (tm/* (tm/+ v (v/polar (/ 200 height) (* noise eq/TAU))) 0.85)]
                    [(tm/+ p v) v'])))
               (take (lifespan))
               (map first)
               (take-while (fn [p] (collide/bounded? bounds p))))]
      (csvg/path
       (into [[:M start]]
             (map (fn [p] [:L p]) path))))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"
              :stroke-width 0.5}
     (let [seed (tm/abs (dr/randvec2 100))
           bounds (rect/rect 0 0 width height)
           scale (dr/rand-nth [(/ 1 400) (/ 1 800) (/ 1 1200)])
           lifespan (dr/weighted {(constantly 100) 1
                                  (constantly 80) 1
                                  (constantly 60) 1
                                  (fn [] (dr/random-int 60 100)) 1})]
       (repeatedly 600 (make-path bounds seed scale lifespan))))))

(sketch/definition velocity-fields
  {:created-at "2023-01-27"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :velocity-fields)
              "sketch-host"))
