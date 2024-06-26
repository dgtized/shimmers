(ns shimmers.sketches.triangulating-subdivisions
  "Playing with concepts from
  https://tylerxhobbs.com/essays/2017/aesthetically-pleasing-triangle-subdivision."
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.probability :as p]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn new-color []
  [(q/random 1) 0.75 0.85 0.5])

(defn drift [[h s l a]]
  (if (p/chance 0.02)
    [(mod (+ h 0.25) 1.0)
     (tm/clamp (+ (* 0.0075 (q/random-gaussian)) s) 0.0 1.0)
     (tm/clamp (+ (* 0.0010 (q/random-gaussian)) l) 0.0 1.0)
     a]
    [(mod (+ (* 0.01 (q/random-gaussian)) h) 1)
     s
     (tm/clamp (+ 0.01 l) 0.0 1.0)
     a]))

(defn dividable? [{:keys [depth max-depth]}]
  (< depth max-depth))

(defn subdivide-triangle [{:keys [color depth max-depth] :as t}]
  (let [opts {:mode (dr/weighted {:midpoint 8
                                  :inset 2
                                  :trisect 2
                                  :centroid 1})
              :inner-point triangle/random-point-inside
              :sample (p/gaussian-clamped 0.5 0.1)
              :sample-low (p/gaussian-clamped 0.33 0.1)
              :sample-high (p/gaussian-clamped 0.66 0.1)}]
    (for [child (triangle/decompose t opts)]
      (assoc child
             :color
             (when (and color (p/chance 0.90))
               (drift color))
             :depth (inc depth)
             :max-depth
             (cond (and (> depth 2.5) (p/chance 0.05))
                   (+ depth 1.5)
                   color
                   (- max-depth 0.2)
                   :else
                   max-depth)))))

(defn make-triangle
  [a b c & {:keys [color depth max-depth]
            :or {depth 0 max-depth 14}}]
  (assoc (gt/triangle2 a b c)
         :color color
         :depth (inc depth)
         :max-depth max-depth))

(defn initialize-shape
  ([triangles] (initialize-shape triangles (math/pow 2 15)))
  ([triangles triangle-limit]
   {:triangles triangles
    :to-draw triangles
    :total (count triangles)
    :triangle-limit triangle-limit}))

(defn one-triangle [w h]
  (let [top (gv/vec2 (* (q/random 0.1 0.9) w) (* 0.1 h))
        left (gv/vec2 (* 0.1 w) (* 0.9 h))
        right (gv/vec2 (* 0.9 w) (* 0.9 h))]
    (initialize-shape
     [(make-triangle top left right :color (new-color) :max-depth 10)])))

(defn split-rectangle [w h]
  (let [a (gv/vec2 (* 0.05 w) (* 0.05 h))
        b (gv/vec2 (* 0.95 w) (* 0.05 h))
        c (gv/vec2 (* 0.05 w) (* 0.95 h))
        d (gv/vec2 (* 0.95 w) (* 0.95 h))]
    (initialize-shape
     [(make-triangle a b c :color [0.55 0.70 0.35 0.9] :max-depth 9)
      (make-triangle c d b :color [0.01 0.85 0.30 0.7] :max-depth 9)])))

(defn empty-rectangle [w h]
  (let [a (gv/vec2 (* 0.05 w) (* 0.05 h))
        b (gv/vec2 (* 0.95 w) (* 0.05 h))
        c (gv/vec2 (* 0.05 w) (* 0.95 h))
        d (gv/vec2 (* 0.95 w) (* 0.95 h))]
    (initialize-shape
     [(make-triangle a b d)
      (make-triangle a c d)]
     (math/pow 2 16))))

(defn subset-rectangle [w h]
  (initialize-shape
   (concat
    (let [a (gv/vec2 (* 0.10 w) (* 0.10 h))
          b (gv/vec2 (* 0.90 w) (* 0.10 h))
          c (gv/vec2 (* 0.10 w) (* 0.50 h))
          d (gv/vec2 (* 0.90 w) (* 0.50 h))]
      [(make-triangle a b d)
       (make-triangle a c d :max-depth 8)])
    (let [a (gv/vec2 (* 0.10 w) (* 0.50 h))
          b (gv/vec2 (* 0.50 w) (* 0.50 h))
          c (gv/vec2 (* 0.10 w) (* 0.90 h))
          d (gv/vec2 (* 0.50 w) (* 0.90 h))]
      [(make-triangle a b c)
       (make-triangle d b c)])
    (let [a (gv/vec2 (* 0.52 w) (* 0.52 h))
          b (gv/vec2 (* 0.92 w) (* 0.52 h))
          c (gv/vec2 (* 0.52 w) (* 0.92 h))
          d (gv/vec2 (* 0.92 w) (* 0.92 h))]
      [(make-triangle a b d :color [0.55 0.35 0.35 1.0] :max-depth 7)
       (make-triangle a c d :color [0.39 0.40 0.35 1.0] :max-depth 4)]))))

(defn initial-conditions []
  (q/background 255)
  (let [shapes [one-triangle
                split-rectangle empty-rectangle
                subset-rectangle]]
    ((rand-nth shapes) (q/width) (q/height))))

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; now dominated by MinorGC and cost of sort?
  (set! (.-disableFriendlyErrors js/p5) true)

  (q/frame-rate 60)
  (q/color-mode :hsl 1.0)
  (initial-conditions))

(defn by-depth [t]
  (:depth t))

(defn subdivide-batch [{:keys [total triangles triangle-limit] :as state}]
  (if (> total triangle-limit)
    [true state]
    (let [[to-divide remaining]
          (->> triangles
               ;; Less randomization from depth, but roughly stays sorted if
               ;; always taking from front and adding to the back.
               ;; (sort-by by-depth)
               (split-at (int (math/pow 1.5 (int (math/log total))))))
          subdivided (mapcat subdivide-triangle to-divide)]
      (if (empty? to-divide)
        [true state]
        [false
         (assoc state
                :total (+ total (count subdivided))
                :triangles (concat remaining (filter dividable? subdivided))
                :to-draw subdivided)]))))

(defn update-state [state]
  (cq/if-steady-state
   state 8
   initial-conditions
   subdivide-batch))

(defn draw [{:keys [to-draw]}]
  (q/stroke 0 0 0 0.5)
  (q/stroke-weight 0.1)
  (doseq [{[a b c] :points color :color} to-draw]
    (q/fill 0 1 1 1.0)
    (cq/color-if q/fill color)
    (cq/draw-triangle a b c)))

(defn page []
  (sketch/component
   :size [900 900]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition triangulating-subdivisions
  {:created-at "2021-02-22"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
