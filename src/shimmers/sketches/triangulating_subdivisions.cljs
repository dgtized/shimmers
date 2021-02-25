(ns shimmers.sketches.triangulating-subdivisions
  "Playing with concepts from
  https://tylerxhobbs.com/essays/2017/aesthetically-pleasing-triangle-subdivision."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [shimmers.common.sequence :as cs]
            [shimmers.common.quil :as quil]))

(defn subdivide-line [p q]
  (geom/point-at (gl/line2 p q) (q/random 0.25 0.75)))

(defn longest-edge [{[a b c] :points}]
  (let [dist-ab (geom/dist a b)
        dist-bc (geom/dist b c)
        dist-ca (geom/dist c a)]
    (cond (and (>= dist-ab dist-bc) (>= dist-ab dist-ca))
          [a b c]
          (and (>= dist-ca dist-bc) (>= dist-ca dist-ab))
          [a c b]
          :else [b c a])))

(defn new-color []
  [(q/random 360) 75 85 0.5])

(defn make-triangle
  [a b c & {:keys [color depth max-depth]
            :or {depth 0 max-depth 14}}]
  (assoc (gt/triangle2 a b c)
         :color color
         :depth (+ depth (* 0.9 (rand)))
         :max-depth max-depth))

(defn drift [[h s l a]]
  (if (< (rand) 0.02)
    [(mod (+ h 90) 360)
     (+ (* 0.75 (q/random-gaussian)) s)
     (+ (* 0.1 (q/random-gaussian)) l)
     (* 0.5 a)]
    [(mod (+ (* 4 (q/random-gaussian)) h) 360) s (+ 1 l) a]))

(defn subdivide-triangle [t]
  (let [[a b c] (longest-edge t)
        distribution (cs/weighted 8 :midpoint
                                  2 :inset
                                  1 :centroid)]
    (case (rand-nth distribution)
      :midpoint
      (let [m (subdivide-line a b)]
        [(make-triangle a m c)
         (make-triangle b m c)])
      :centroid
      (let [inner (geom/random-point-inside t)]
        [(make-triangle a b inner)
         (make-triangle b c inner)
         (make-triangle c a inner)])
      :inset
      (let [mab (subdivide-line a b)
            mbc (subdivide-line b c)
            mca (subdivide-line c a)]
        [(make-triangle a mab mca)
         (make-triangle b mab mbc)
         (make-triangle c mbc mca)
         (make-triangle mab mbc mca)]))))

(defn dividable? [{:keys [depth max-depth]}]
  (< depth max-depth))

(defn subdivide [{:keys [color depth max-depth] :as s}]
  (for [child (subdivide-triangle s)]
    (assoc child
           :color
           (when (and color (< (rand) 0.90))
             (drift color))
           :depth (inc depth)
           :max-depth
           (if (and (> depth 2.5) (< (rand) 0.05))
             (+ depth 1.5)
             max-depth))))

(defn one-triangle [w h]
  (let [top (v/vec2 (* (q/random 0.1 0.9) w) (* 0.1 h))
        left (v/vec2 (* 0.1 w) (* 0.9 h))
        right (v/vec2 (* 0.9 w) (* 0.9 h))]
    [(make-triangle top left right :color (new-color) :max-depth 10)]))

(defn split-rectangle [w h]
  (let [a (v/vec2 (* 0.05 w) (* 0.05 h))
        b (v/vec2 (* 0.95 w) (* 0.05 h))
        c (v/vec2 (* 0.05 w) (* 0.95 h))
        d (v/vec2 (* 0.95 w) (* 0.95 h))]
    [(make-triangle a b c :color [200 70 35 0.9] :max-depth 9)
     (make-triangle c d b :color [5 85 30 0.7] :max-depth 9)]))

(defn empty-rectangle [w h]
  (let [a (v/vec2 (* 0.05 w) (* 0.05 h))
        b (v/vec2 (* 0.95 w) (* 0.05 h))
        c (v/vec2 (* 0.05 w) (* 0.95 h))
        d (v/vec2 (* 0.95 w) (* 0.95 h))]
    [(make-triangle a b d)
     (make-triangle a c d)]))

(defn subset-rectangle [w h]
  (concat
   (let [a (v/vec2 (* 0.10 w) (* 0.10 h))
         b (v/vec2 (* 0.90 w) (* 0.10 h))
         c (v/vec2 (* 0.10 w) (* 0.50 h))
         d (v/vec2 (* 0.90 w) (* 0.50 h))]
     [(make-triangle a b d)
      (make-triangle a c d :max-depth 8)])
   (let [a (v/vec2 (* 0.10 w) (* 0.50 h))
         b (v/vec2 (* 0.50 w) (* 0.50 h))
         c (v/vec2 (* 0.10 w) (* 0.90 h))
         d (v/vec2 (* 0.50 w) (* 0.90 h))]
     [(make-triangle a b c)
      (make-triangle d b c)])
   (let [a (v/vec2 (* 0.52 w) (* 0.52 h))
         b (v/vec2 (* 0.92 w) (* 0.52 h))
         c (v/vec2 (* 0.52 w) (* 0.92 h))
         d (v/vec2 (* 0.92 w) (* 0.92 h))]
     [(make-triangle a b d :color [200 35 35 1.0] :max-depth 7)
      (make-triangle a c d :color [140 40 35 1.0] :max-depth 4)])))

(defn initial-conditions []
  (q/background 255)
  (let [shapes [one-triangle
                split-rectangle empty-rectangle
                subset-rectangle]
        triangles ((rand-nth shapes) (q/width) (q/height))]
    {:triangles triangles
     :total (count triangles)}))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsl 360 100.0 100.0 1.0)
  (initial-conditions))

(defn by-depth [t]
  (:depth t))

(defn subdivide-batch [{:keys [total triangles] :as state}]
  (if (> total (Math/pow 2 15))
    [true state]
    (let [[to-divide remaining]
          (->> triangles
               (filter dividable?)
               (sort-by by-depth)
               (split-at 48))
          subdivided (mapcat subdivide to-divide)]
      (if (empty? to-divide)
        [true state]
        [false
         (assoc state
                :total (+ total (count subdivided))
                :triangles (concat subdivided remaining)
                :to-draw subdivided)]))))

(defn update-state [state]
  (quil/if-steady-state
   state 8
   initial-conditions
   subdivide-batch))

(defn draw-triangle [a b c]
  (q/triangle (:x a) (:y a) (:x b) (:y b) (:x c) (:y c)))

(defn draw [{:keys [triangles to-draw]}]
  (q/stroke 0 0 0 0.5)
  (q/stroke-weight 0.1)
  (doseq [{[a b c] :points color :color}
          (if (seq to-draw) to-draw triangles)]
    (q/fill 0 100 100 1.0)
    (when color
      (apply q/fill color))
    (draw-triangle a b c)))

(defn ^:export run-sketch []
  (q/defsketch triangulating-subdivisions
    :host "quil-host"
    :size [600 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
