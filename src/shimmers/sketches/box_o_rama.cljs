(ns shimmers.sketches.box-o-rama
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]))

(defn random-box [s]
  (g/scale (rect/rect 0 0 (dr/random-int 50 100) (dr/random-int 50 100)) s))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:boxes [(g/center (random-box 1.0) (cq/rel-vec 0.5 0.5))]})

(defn generate-box [boxes]
  (let [scale (- 1.0 (/ (count boxes) 50))
        side (dr/rand-nth [:right :left :top :bottom])
        fixed (dr/rand-nth boxes)
        box (random-box scale)
        placed (square/align-to side 3.0 fixed (g/center box (:p fixed)))]
    (when (geometry/contains-box? (cq/screen-rect 0.95) placed)
      placed)))

(defn update-state [{:keys [boxes] :as state}]
  (if (> (count boxes) 30)
    state
    (if-let [box (generate-box boxes)]
      (update state :boxes conj box)
      state)))

(defn draw [{:keys [boxes]}]
  (q/background 1.0)
  (q/stroke 0)
  (q/stroke-weight 0.8)
  (q/no-fill)
  (doseq [shape boxes]
    (qdg/draw shape)))

(sketch/defquil box-o-rama
  :created-at "2021-"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
