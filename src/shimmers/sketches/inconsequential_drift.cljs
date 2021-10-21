(ns shimmers.sketches.inconsequential-drift
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn triangle [p w h]
  (-> (gt/equilateral2 0 0 w h)
      (g/center p)
      (g/scale-size 1.5)))

(defn rectangle [p w h]
  (-> (rect/rect 0 0 w h)
      (g/center p)
      (g/scale-size 1.5)))

(defn square-grid [size]
  (for [x (range size)
        y (range size)]
    (let [px (/ x size)
          py (/ y size)
          noise-xy (q/noise (/ x 4) (/ y 4))]
      {:pos (tm/+ (gv/vec2 x y) (dr/jitter (* 0.1 noise-xy)))
       :color (dr/weighted {[0.0 0.7 0.45 0.45] (- 1.0 (* px py))
                            [0.30 0.7 0.45 0.45] (- 1.0 px)
                            [0.60 0.7 0.45 0.45] (- 1.0 py)})
       :rotate noise-xy
       :shape (dr/weighted {:ellipse 8 :triangle (* 2 px) :rectangle py})
       :width (dr/random 0.3 (max 0.45 (* 0.9 px)))
       :height (dr/random 0.3 (max 0.45 (* 0.9 py)))})))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/noise-seed (dr/random tm/MAX))
  (q/no-loop)
  (let [size 64]
    {:size size
     :grid (square-grid size)}))

(defn draw [{:keys [size grid]}]
  (q/background 1.0 0.5)
  (q/no-stroke)
  (q/ellipse-mode :radius)
  (let [scale (/ (q/width) (+ size 2))
        base (gv/vec2 (* scale 1.5) (* scale 1))]
    (doseq [{:keys [shape pos rotate color width height]} grid
            :let [p (tm/+ base (tm/* pos scale))
                  [x y] p
                  w (* width scale 0.85)
                  h (* height scale 0.85)]]
      (q/with-rotation [(/ rotate 20)]
        (apply q/fill color)
        (case shape
          :ellipse
          (q/ellipse x y w h)
          :triangle
          (cq/draw-polygon (triangle p w h))
          :rectangle
          (cq/draw-polygon (rectangle p w h)))))))

(sketch/defquil inconsequential-drift
  :created-at "2021-06-13"
  :tags #{:static :deterministic}
  :size [800 800]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
