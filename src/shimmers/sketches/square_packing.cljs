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

(def PHI (/ (+ 1 (Math/sqrt 5)) 2))

(defn split-panes
  [{:keys [p size]} square [px py]]
  (let [[width height] size
        offset-x (* px (- width square))
        offset-y (* py (- height square))
        sq (rect/rect (tm/+ p [offset-x offset-y]) square square)]
    (if (> height width)
      ;; row major
      [sq
       (rect/rect p width offset-y) ;; south row
       (rect/rect (tm/+ p [0 (+ square offset-y)]) width (- height square offset-y)) ;; north row
       (rect/rect (tm/+ p [0 offset-y]) offset-x square) ;; east chunk
       (rect/rect (tm/+ p [(+ offset-x square) offset-y]) (- width square offset-x) square) ;; west chunk
       ]
      ;; column major
      [sq
       (rect/rect (tm/+ p [offset-x 0]) square offset-y) ;; south chunk
       (rect/rect (tm/+ p [offset-x (+ square offset-y)]) square (- height square offset-y)) ;; north chunk
       (rect/rect p offset-x height) ;; east column
       (rect/rect (tm/+ p [(+ square offset-x) 0]) (- width square offset-x) height) ;; west column
       ])))

(defn has-area? [{:keys [size]}]
  (every? pos? size))

(defn pack [rectangle ratio]
  (let [{:keys [size]} rectangle
        [w h] size
        square (* (min w h) ratio)
        [px py] (repeatedly 2 #(p/weighted {0.0 1.0
                                            0.5 1.0
                                            1.0 1.0}))]
    (filter has-area? (split-panes rectangle square [px py]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:squares []
   :remaining [(rect/rect [10 10] [790 590])]})

(defn random-ratio []
  (p/weighted {(/ 1 PHI) 4
               0.5 2
               (/ 1 3) 2}))

(defn update-state [{:keys [remaining squares] :as state}]
  (if (and (not-empty remaining) (< (count squares) 32))
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
