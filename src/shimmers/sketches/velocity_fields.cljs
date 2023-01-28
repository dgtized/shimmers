(ns shimmers.sketches.velocity-fields
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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
               (take-while (fn [p] (g/contains-point? bounds p))))]
      (when (seq path)
        (csvg/path
         (into [[:M start]]
               (map (fn [p] [:L p]) path)))))))

(defn screen-rect []
  (rect/rect 0 0 width height))

(defn triplet []
  (let [center (rv 0.5 0.5)
        base-angle (dr/random eq/TAU)
        triangles (dr/chance 0.5)]
    (map (fn [i]
           (let [angle (+ base-angle (* i 1.1 tm/PHI))
                 p (v/+polar center (* 0.33 height) angle)
                 max-radius (g/dist p (g/closest-point (screen-rect) p))
                 circle (gc/circle p (min (* 0.33 height (/ 1 (inc i)))
                                          max-radius))]
             (if triangles
               circle
               (triangle/inscribed-equilateral
                circle
                (- eq/TAU angle)))))
         (range 3))))

(defn boundaries []
  (dr/rand-nth [(g/scale-size (rect/rect 0 0 width height) 0.8)
                (g/translate
                 (geometry/rotate-around-centroid
                  (g/scale-size (rect/rect 0 0 width height) 0.66)
                  (dr/random -0.5 0.5))
                 (dr/randvec2 (* 0.1 height)))
                (gc/circle (rv (dr/rand-nth [0.4 0.5 0.6]) 0.5) (* 0.45 height))
                (-> (rv (dr/rand-nth [0.4 0.5 0.6]) 0.5)
                    (gc/circle (* 0.6 height))
                    (triangle/inscribed-equilateral (dr/random eq/TAU)))
                (gc/circle (rv (dr/rand-nth [0.4 0.5 0.6]) 0.5) (* 0.6 height))]))

(defn shape-plan []
  (let [gen (dr/weighted {(fn [] [(screen-rect)]) 1
                          (fn [] (repeatedly 1 boundaries)) 2
                          (fn [] (repeatedly 2 boundaries)) 2
                          (fn [] (repeatedly 3 boundaries)) 1
                          (fn [] [(screen-rect) (boundaries)]) 2
                          (fn [] (triplet)) 2
                          (fn [] (into [(screen-rect)] (triplet))) 3})]
    (gen)))

;; exclude full rectangle if first shape?
(defn shapes [seed scale bounds n]
  (let [lifespan (dr/weighted {(constantly 100) 1
                               (constantly 80) 1
                               (constantly 60) 1
                               (fn [] (dr/random-int 60 100)) 1})]
    (->> (make-path bounds seed scale lifespan)
         repeatedly
         (keep identity)
         (take (max 100 (int (* n (/ (g/area bounds) (* width height)))))))))

;; interesting pattern with big circle left, small circle right, flow field heading right
;; http://localhost:9500/#/sketches/velocity-fields?seed=4000107855
(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"
              :stroke-width 0.5}
     (let [seed (tm/abs (dr/randvec2 100))
           scale (dr/rand-nth [(/ 1 400) (/ 1 800) (/ 1 1200)])
           offset (dr/weighted {0 2 10 2 15 1})
           n (dr/rand-nth [600 900 1200])]
       (mapcat (fn [bounds i] (shapes (tm/+ seed (dr/randvec2 (* i offset scale)))
                                     scale
                                     bounds
                                     (+ n (* i 600))))
               (shape-plan)
               (range))))))

(defn ui-controls []
  [:div
   [:div.readable-width
    [:p
     "Flow fields generated using a simple physics engine, applying a fixed
      acceleration at each point in the direction of the noise field and then
      keeping velocity in check by applying a drag coefficent."]
    [:p
     "Some outputs apply the algorithm twice using two bounding shapes but
     idential noise scale and seed, which varies the density and adds some
     interesting edges."]]])

(sketch/definition velocity-fields
  {:created-at "2023-01-27"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :velocity-fields
                                    ui-controls)
              "sketch-host"))