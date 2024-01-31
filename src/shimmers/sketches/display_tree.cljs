(ns shimmers.sketches.display-tree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.core :as g]))

(defn generate-box [{p :p [width height] :size}]
  (let [[w h] (dr/weighted {(gv/vec2 4 3) 1
                            (gv/vec2 3 2) 1})
        scale (dr/gaussian (/ height 10) (/ height 240))
        box (gv/vec2 (* w scale) (* h scale))
        [x y] p
        a (gv/vec2 (dr/random x (- width (:x box)))
                   (dr/random y (- height (:y box))))]
    (rect/rect a (tm/+ a box))))

(defn place-boxes [bounds]
  (loop [boxes [] attempts 0]
    (cond (> (count boxes) 6)
          boxes
          (> attempts 512)
          (recur [] 0)
          :else
          (let [candidate (generate-box bounds)]
            (if (some (fn [b] (collide/overlaps? (g/scale-size b 1.15) candidate))
                      boxes)
              (recur boxes (inc attempts))
              (recur (conj boxes candidate) (inc attempts)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:displays (place-boxes (cq/screen-rect 0.8))})

(defn update-state [state]
  state)

(defn draw [{:keys [displays]}]
  (q/background 1.0)
  (doseq [screen displays]
    (qdg/draw screen)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition display-tree
  {:created-at "2024-01-30"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
