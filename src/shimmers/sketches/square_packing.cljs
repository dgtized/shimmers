(ns shimmers.sketches.square-packing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.probability :as p]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as geom]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

;; Further Experiments: pack resulting squares with patterns of their own?
;; Colors and shapes, even tilted or "hand drawn" squares?

;; Note that px,py are not clamped to 0,1 so some funky but interesting results
;; are possible if using values outside of the range.
(defn split-panes
  "Split a rectangle into a square and the 4 surrounding rectangles. The square is
  of `size`, with `px,py` indicating percent positioning within the
  larger rectangle. row-major indicates if the panes should split by rows and
  then fill in the gaps east and west of the square, or by columns and fill in
  the gaps north or south of the square."
  ([{[w h] :size :as rectangle} size percent]
   (split-panes rectangle size percent (> h w)))
  ([{p :p [width height] :size} size [px py] row-major]
   (let [offset-x (* px (- width size))
         offset-y (* py (- height size))
         sq (rect/rect (tm/+ p [offset-x offset-y]) size size)]
     (->> (if row-major
            [(rect/rect p width offset-y) ;; south row
             (rect/rect (tm/+ p [0 (+ size offset-y)]) width (- height size offset-y)) ;; north row
             (rect/rect (tm/+ p [0 offset-y]) offset-x size) ;; east chunk
             (rect/rect (tm/+ p [(+ offset-x size) offset-y]) (- width size offset-x) size) ;; west chunk
             ]
            [(rect/rect (tm/+ p [offset-x 0]) size offset-y) ;; south chunk
             (rect/rect (tm/+ p [offset-x (+ size offset-y)]) size (- height size offset-y)) ;; north chunk
             (rect/rect p offset-x height) ;; east column
             (rect/rect (tm/+ p [(+ size offset-x) 0]) (- width size offset-x) height) ;; west column
             ])
          (into [sq])))))

(defn has-area? [{:keys [size]}]
  (every? pos? size))

(defn middle-out
  "Alternate distribution for px,py"
  []
  (p/weighted {0.0 1.0
               0.5 1.0
               1.0 1.0}))

(defn pack [rectangle ratio]
  (let [{:keys [size]} rectangle
        [w h] size
        square (* (min w h) ratio)
        [px py] (repeatedly 2 (fn [] (mod (* tm/PHI (rand)) 1.0)))]
    (filter has-area? (split-panes rectangle square [px py]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:squares []
   :remaining [(rect/rect [10 10] [790 590])]})

(defn random-ratio []
  (p/weighted {(/ 1 tm/PHI) 4
               0.5 2
               (/ 1 3) 2}))

(defn update-state [{:keys [remaining squares] :as state}]
  (if (and (not-empty remaining) (< (count squares) 256))
    (let [rect (p/weighted-by geom/area remaining)
          [s & r] (pack rect (/ 1 tm/PHI))]
      (-> state
          (assoc :remaining (into (remove #{rect} remaining) r))
          (update :squares conj s)))
    state))

(defn draw [{:keys [squares remaining]}]
  (q/background 1.0)
  (q/stroke-weight 0.66)
  (q/stroke 0.35 0.5 0.5 0.5)
  (q/fill 1.0 1.0)
  (doseq [rects remaining]
    (cq/draw-shape (geom/vertices rects)))
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/fill 1.0 0.1)
  (doseq [square squares]
    (cq/draw-shape (geom/vertices (geom/scale-size square (/ 1 tm/PHI))))))

(sketch/defquil square-packing
  :created-at "2021-10-17"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
