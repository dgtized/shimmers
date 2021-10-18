(ns shimmers.sketches.square-packing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as geom]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.probability :as p]))

(def PHI (/ (+ 1 (Math/sqrt 5)) 2))

(defn split-x [{:keys [p size]} square pct]
  (let [[width height] size
        offset-x (* pct (- width square))]
    [(rect/rect (tm/+ p [offset-x 0]) square square)
     (rect/rect p offset-x square) ;; before-x
     (rect/rect (tm/+ p [(+ square offset-x) 0]) (- width square offset-x) square) ;; after-x
     (rect/rect (tm/+ p [0 square]) width (- height square)) ;; y sliver
     ]))

(defn split-y [{:keys [p size]} square pct]
  (let [[width height] size
        offset-y (* pct (- height square))]
    [(rect/rect (tm/+ p [0 offset-y]) square square)
     (rect/rect p square offset-y) ;; before-y
     (rect/rect (tm/+ p (gv/vec2 0 (+ square offset-y))) square (- height square offset-y)) ;; after-y
     (rect/rect (tm/+ p (gv/vec2 square 0)) (- width square) height) ;; x sliver
     ]))

(defn has-area? [{:keys [size]}]
  (every? pos? size))

(defn pack [rectangle ratio]
  (let [{:keys [size]} rectangle
        [w h] size
        square (* (min w h) ratio)
        split (p/weighted {split-x w
                           split-y h})]
    (filter has-area? (split rectangle square (rand-nth [0.0 0.5 1.0])))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:squares []
   :remaining [(rect/rect [10 10] [790 590])]})

(defn random-ratio []
  (p/weighted {(/ 1 PHI) 4
               0.5 2
               (/ 1 3) 2}))

(defn update-state [{:keys [remaining] :as state}]
  (if (and (not-empty remaining) (< (count remaining) 64))
    (let [rect (p/weighted-by geom/area remaining)
          [s & r] (pack rect (/ 1 PHI))]
      (-> state
          (assoc :remaining (into (remove #{rect} remaining) r))
          (update :squares conj s)))
    state))

(defn draw [{:keys [squares remaining]}]
  (q/background 1.0)
  (q/stroke 0.35 0.5 0.5 1.0)
  (q/fill 1.0 1.0)
  (doseq [rects remaining]
    (cq/draw-shape (geom/vertices rects)))
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/fill 1.0 0.1)
  (doseq [square squares]
    (cq/draw-shape (geom/vertices square))))

(sketch/defquil square-packing
  :created-at "2021-10-17"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
