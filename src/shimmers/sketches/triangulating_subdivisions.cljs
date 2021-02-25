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
            [shimmers.common.sequence :as cs]))

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

(defn add-color [t color]
  (if color
    (assoc t :color color)
    t))

(defn drift [[h s l a]]
  (if (< (rand) 0.05)
    [(mod (+ h 90) 360)
     (+ (* 2 (q/random-gaussian)) s)
     (+ (* 2 (q/random-gaussian)) l)
     (* 0.5 a)]
    [(mod (+ (* 8 (q/random-gaussian)) h) 360) s (+ 1 l) a]))

(defn map-colors [color triangles]
  (for [t triangles]
    (if (and color (< (rand) 0.90))
      (add-color t (drift color))
      t)))

(defn subdivide-triangle [t]
  (let [[a b c] (longest-edge t)
        distribution (cs/weighted 8 :midpoint
                                  2 :inset
                                  1 :centroid)]
    (case (rand-nth distribution)
      :midpoint
      (let [m (subdivide-line a b)]
        [(gt/triangle2 a m c)
         (gt/triangle2 b m c)])
      :centroid
      (let [inner (geom/random-point-inside t)]
        [(gt/triangle2 a b inner)
         (gt/triangle2 b c inner)
         (gt/triangle2 c a inner)])
      :inset
      (let [mab (subdivide-line a b)
            mbc (subdivide-line b c)
            mca (subdivide-line c a)]
        [(gt/triangle2 a mab mca)
         (gt/triangle2 b mab mbc)
         (gt/triangle2 c mbc mca)
         (gt/triangle2 mab mbc mca)]))))

(defn one-triangle [w h]
  (let [top (v/vec2 (* (q/random 0.1 0.9) w) (* 0.1 h))
        left (v/vec2 (* 0.1 w) (* 0.9 h))
        right (v/vec2 (* 0.9 w) (* 0.9 h))]
    [(add-color (gt/triangle2 top left right) (new-color))]))

(defn split-rectangle [w h]
  (let [a (v/vec2 (* 0.05 w) (* 0.05 h))
        b (v/vec2 (* 0.95 w) (* 0.05 h))
        c (v/vec2 (* 0.05 w) (* 0.95 h))
        d (v/vec2 (* 0.95 w) (* 0.95 h))]
    [(add-color (gt/triangle2 a b c) [180 60 70 0.8])
     (add-color (gt/triangle2 c d b) [0 60 70 0.8])]))

(defn initial-conditions []
  (let [shape-fn (rand-nth [one-triangle split-rectangle])]
    {:triangles (shape-fn (q/width) (q/height))}))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsl 360 100.0 100.0 1.0)
  (initial-conditions))

(defn area [t]
  (* 0.5 (geom/height t) (geom/width t)))

(defn update-state [{:keys [triangles] :as state}]
  (if (> (count triangles) 3000)
    (initial-conditions)
    ;; bias towards subdividing largest triangles
    (let [ordered (sort-by area triangles)
          cutoff (int (* 0.33 (count triangles)))
          batch 8
          randomized (shuffle (drop cutoff ordered))
          divisions
          (mapcat (fn [s] (map-colors (:color s) (subdivide-triangle s)))
                  (take batch randomized))]
      (assoc state :triangles (into (take cutoff ordered)
                                    (into divisions (drop batch randomized)))))))

(defn draw-triangle [a b c]
  (q/triangle (:x a) (:y a) (:x b) (:y b) (:x c) (:y c)))

(defn draw [{:keys [triangles]}]
  (q/background 255)
  (q/stroke-weight 0.05)
  (doseq [{[a b c] :points color :color} triangles]
    (if color
      (apply q/fill color)
      (q/no-fill))
    (draw-triangle a b c)))

(defn ^:export run-sketch []
  (q/defsketch triangulating-subdivisions
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
