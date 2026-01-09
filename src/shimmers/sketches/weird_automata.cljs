(ns shimmers.sketches.weird-automata
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.core :as g]))

(defn center-box [{[w h] :size :as bounds}]
  (let [s (min w h)]
    (g/center (rect/rect 0 0 s s) (g/centroid bounds))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 6)
  (let [n 48]
    {:n n
     :grid
     (into {} (for [x (range n)
                    y (range n)]
                [[x y] (dr/chance 0.25)]))}))

(defn xor [a b]
  (bit-xor (if a 1 0) (if b 1 0)))

(defn neighbors [grid cell]
  (for [r [-1 0 1]
        c [-1 0 1]
        :let [v (get grid (mapv + cell [r c]) nil)]
        :when v]
    [[r c] v]))

(defn step [grid]
  (reduce-kv (fn [g cell _]
               (let [neighborhood (neighbors grid cell)
                     values (count (keep true? (mapv second neighborhood)))]
                 (assoc g cell (< 0.2 (/ values 9.0) 0.85))))
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
      (q/fill (if on 1.0 0.0))
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
    [:p "Uses a conway's life sorta rule but instead based on the threshold of " [:em "on"] " values surrounding the cell. It stays on if (< 0.2 (/ neighbors 9.0) 0.85)."]
    ]])

(sketch/definition weird-automata
  {:created-at "2026-01-09"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))
