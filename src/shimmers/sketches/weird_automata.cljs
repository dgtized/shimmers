(ns shimmers.sketches.weird-automata
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

(defn center-box [{[w h] :size :as bounds}]
  (let [s (min w h)]
    (g/center (rect/rect 0 0 s s) (g/centroid bounds))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 8)
  (let [n 64]
    {:n n
     :grid
     (into {} (for [x (range n)
                    y (range n)]
                [[x y] (dr/random 0.1 0.9)]))}))

(defn neighbors [grid cell]
  (for [r [-1 0 1]
        c [-1 0 1]
        :let [v (get grid (mapv + cell [r c]) nil)]
        :when v]
    [[r c] v]))

(defn step [grid]
  (reduce-kv (fn [g cell value]
               (let [neighborhood (neighbors grid cell)
                     avg (/ (reduce + (mapv second neighborhood))
                            (+ 1 (count neighborhood)))]
                 (assoc g cell
                        (tm/mix* value (if (< 0.2 avg 0.85) 1.0 0.0)
                                 (+ 0.05 (* 0.65 (tm/smoothstep* 0.25 0.85 avg)))))))
             grid
             grid))

(defn update-state [state]
  (update state :grid step))

(defn draw [{:keys [grid n]}]
  (q/background 1.0)
  (q/no-stroke)
  (let [{[cx cy] :p [width height] :size} (center-box (cq/screen-rect 0.975))
        scale 0.85
        w (int (/ (float width) n))
        h (int (/ (float height) n))
        off (/ (- 1.0 scale) 2.0)]
    (doseq [[[x y] on] grid]
      (q/fill on)
      (q/rect (+ cx (* (+ off x) w)) (+ cy (* (+ off y) h)) (* scale w) (* scale h)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.centered.readable-width
    [:p "Genuary 2026 - Day 9 - Weird Cellular Automata"]
    [:p "Floating point automata, where each location turns on or off based on
    the average value of the neighboring 9 cells adjusted based on how full that
    region is."] ]])

(sketch/definition weird-automata
  {:created-at "2026-01-09"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))
