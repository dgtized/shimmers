(ns shimmers.sketches.box-o-rama
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

(defn random-box [s]
  (g/scale (rect/rect 0 0 (dr/random-int 50 100) (dr/random-int 50 100)) s))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:boxes [(g/center (random-box 1.0) (cq/rel-vec 0.5 0.5))]})

(defn percent-intersect [shape box]
  (/ (g/area (tm/intersection shape box))
     (g/area box)))

(defn generate-box [boxes]
  (let [scale (- 1.0 (/ (count boxes) 48))
        side (dr/weighted {:right 4.0
                           :left 4.0
                           :top 1.0
                           :bottom 1.0})
        fixed (dr/rand-nth boxes)
        box (random-box scale)
        fixed-pos (dr/rand-nth [(:p fixed) (rect/top-right fixed) (g/centroid fixed)])
        placed (square/align-to side 5.0 fixed (g/center box fixed-pos))
        overlaps (filter #(g/intersect-shape % placed) boxes)
        percent (if (seq overlaps)
                  (percent-intersect (first overlaps) placed)
                  0)]
    (when (and (geometry/contains-box? (cq/screen-rect 0.9) placed)
               (< (count overlaps) 2)
               (<= percent 0.1))
      (let [theta (g/heading (tm/- fixed-pos (:p placed)))
            t (* 0.03 theta)
            o (g/centroid placed)]
        (assoc placed
               :theta t
               :hatching
               (when (dr/chance 0.4)
                 (mapv #(geometry/rotate-around % o t)
                       (clip/hatch-rectangle placed (* 6.0 scale) theta [0.5 0.5]))))))))

(defn update-state [{:keys [boxes] :as state}]
  (if (> (count boxes) 32)
    state
    (if-let [box (generate-box boxes)]
      (update state :boxes conj box)
      state)))

(defn draw [{:keys [boxes]}]
  (q/background 1.0)
  (q/stroke 0)
  (q/stroke-weight 0.8)
  (q/no-fill)
  (doseq [shape boxes
          :let [d (/ (g/dist (g/centroid shape) (cq/rel-vec 0.5 0.5))
                     (g/dist (cq/rel-vec 0.5 0.5) (cq/rel-vec 0 0)))
                weight (- 0.8 (* 0.5 d))]]
    (q/stroke-weight weight)
    (qdg/draw (geometry/rotate-around-centroid shape (get shape :theta 0)))
    (q/stroke-weight (* 0.66 weight))
    (when-let [{:keys [hatching]} shape]
      (doseq [{[p q] :points} hatching]
        (q/line p q)))))

(sketch/defquil box-o-rama
  :created-at "2021-11-29"
  :tags #{:deterministic}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
