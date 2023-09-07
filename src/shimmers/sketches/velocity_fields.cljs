(ns shimmers.sketches.velocity-fields
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-path [bounds seed scale buzzy pareto-width lifespan]
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
               (map (fn [p] [:T p])
                    (dr/random-sample buzzy path)))
         {:stroke-width
          (if pareto-width
            (tm/clamp (/ (dr/pareto 1.0 1.5) 10.0) 0.25 2.0)
            0.5)})))))

(defn screen-rect []
  (rect/rect 0 0 width height))

(defn triplet []
  (let [center (rv 0.5 0.5)
        base-angle (dr/random-tau)
        triangles (dr/chance 0.5)]
    (map (fn [i]
           (let [angle (+ base-angle (* i 1.1 tm/PHI))
                 p (v/+polar center (* 0.33 height) angle)
                 max-radius (poly/dist-to-closest-point (screen-rect) p)
                 circle (gc/circle p (min (* 0.33 height (/ 1 (inc i)))
                                          max-radius))]
             (if triangles
               circle
               (triangle/inscribed-equilateral
                circle
                (- eq/TAU angle)))))
         (range 3))))

(defn facing-pair []
  [(triangle/inscribed-equilateral {:p (rv 0.25 0.5) :r (* 0.37 height)} 0.0)
   (triangle/inscribed-equilateral {:p (rv 0.75 0.5) :r (* 0.37 height)} Math/PI)])

(defn inner []
  (dr/weighted
   [[(g/scale-size (rect/rect 0 0 width height) 0.8) 1]
    [(g/translate
      (geometry/rotate-around-centroid
       (g/scale-size (rect/rect 0 0 width height) 0.66)
       (* eq/TAU (dr/random -0.1 0.1)))
      (dr/randvec2 (* 0.05 height))) 1]
    [(gc/circle (rv (dr/rand-nth [0.4 0.5 0.6]) 0.5) (* 0.45 height)) 1]
    [(-> (rv (dr/rand-nth [0.4 0.5 0.6]) 0.5)
         (gc/circle (* 0.6 height))
         (triangle/inscribed-equilateral (dr/random-tau))) 1]
    [(gc/circle (rv (dr/rand-nth [0.4 0.5 0.6]) 0.5) (* 0.6 height)) 1]]))

;; TODO: bias plan to ensure boundaries are nested and only overlap once
(defn shape-plan []
  ((dr/weighted {(fn [] [(screen-rect)]) 1
                 (fn [] (into [(screen-rect)] (triplet))) 3
                 (fn [] [(screen-rect) (inner)]) 2
                 (fn [] (into [(screen-rect)] (facing-pair))) 1
                 (fn [] (facing-pair)) 1
                 (fn [] (into [(g/scale-size (screen-rect) 0.9)] (triplet))) 1.5
                 (fn [] (triplet)) 1})))

;; exclude full rectangle if first shape?
;; TODO: bias lifespan if buzzy/pareto is enabled?
(defn shapes [seed scale bounds {:keys [buzzy pareto-width]} n]
  (let [lifespan (dr/weighted [[(constantly 150) 1]
                               [(constantly 100) 1]
                               [(constantly 80) 1]
                               [(constantly 60) 1]
                               [(constantly 40) 1]
                               [(fn [] (dr/random-int 50 100)) 1]
                               [(fn [] (dr/random-int 10 50)) 1]])
        ;; TODO: increase bias for buzziness and pareto per inner shape?
        buzzy (if buzzy
                (dr/weighted {1.0 10
                              0.66 2
                              0.5 1})
                1.0)]
    (->> (make-path bounds seed scale buzzy pareto-width lifespan)
         repeatedly
         (keep identity)
         (take (max 100 (int (* n (/ (g/area bounds) (* width height)))))))))

(defn generate-rules []
  (let [scaling (dr/weighted {400 1
                              600 1
                              800 1
                              1000 2
                              1200 1
                              1400 1})
        [buzzy pareto-width]
        (dr/weighted {[false false] 1
                      [true false] 2
                      [false true] 4
                      [true true] 8})]
    {:seed (tm/abs (dr/randvec2 100))
     :scaling scaling
     :scale (/ 1.0 scaling)
     :offset (dr/weighted {0 2 4 2 8 2 12 1})
     :density (dr/rand-nth [600 900 1200])
     :buzzy buzzy
     :pareto-width pareto-width}))

;; interesting pattern with big circle left, small circle right, flow field heading right
;; http://localhost:9500/#/sketches/velocity-fields?seed=4000107855
(defn scene [{:keys [seed scale offset density buzzy pareto-width]}]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"}
    (mapcat (fn [bounds i]
              (shapes (tm/+ seed (dr/randvec2 (* i offset scale)))
                      scale
                      bounds
                      {:buzzy (and buzzy (dr/chance 0.8))
                       :pareto-width (and pareto-width (dr/chance 0.9))}
                      (* density (Math/pow 2.5 i))))
            (shape-plan)
            (range))))

(defn explanation []
  [:div.readable-width
   [:p
    "Flow fields generated using a simple physics engine, applying a fixed
     acceleration at each point in the direction of the noise field and then
     keeping velocity in check by applying a drag coefficent."]
   [:p
    "Some outputs apply the algorithm twice using two bounding shapes but
     idential noise scale and seed, which varies the density and adds some
     interesting edges."]])

(defn page []
  (let [rules (generate-rules)]
    (fn []
      [:<>
       [:div.canvas-frame [scene rules]]
       [:div.contained
        [:div.flexcols {:style {:justify-content :space-evenly}}
         [:div
          [view-sketch/generate :velocity-fields]
          [:p]
          [debug/pre-edn (dissoc rules :seed :scale)]]
         [explanation]]]])))

(sketch/definition velocity-fields
  {:created-at "2023-01-27"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (page)))
