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

(defn make-triangle [a b c & {:keys [color depth] :or {depth 0}}]
  (-> (gt/triangle2 a b c)
      (add-color color)
      (assoc :depth depth)))

(defn drift [[h s l a]]
  (if (< (rand) 0.05)
    [(mod (+ h 90) 360)
     (+ (* 2 (q/random-gaussian)) s)
     (+ (* 2 (q/random-gaussian)) l)
     (* 0.5 a)]
    [(mod (+ (* 8 (q/random-gaussian)) h) 360) s (+ 1 l) a]))

(defn map-colors [color depth triangles]
  (for [t (map (fn [x] (assoc x :depth depth)) triangles)]
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

(defn subdivide [t]
  (map-colors (:color t) (inc (:depth t))
              (subdivide-triangle t)))

(defn one-triangle [w h]
  (let [top (v/vec2 (* (q/random 0.1 0.9) w) (* 0.1 h))
        left (v/vec2 (* 0.1 w) (* 0.9 h))
        right (v/vec2 (* 0.9 w) (* 0.9 h))]
    [(make-triangle top left right :color (new-color))]))

(defn split-rectangle [w h]
  (let [a (v/vec2 (* 0.05 w) (* 0.05 h))
        b (v/vec2 (* 0.95 w) (* 0.05 h))
        c (v/vec2 (* 0.05 w) (* 0.95 h))
        d (v/vec2 (* 0.95 w) (* 0.95 h))]
    [(make-triangle a b c :color [180 60 70 0.8])
     (make-triangle c d b :color [0 60 70 0.8])]))

(defn initial-conditions []
  (let [shape-fn (rand-nth [one-triangle split-rectangle])]
    {:triangles (shape-fn (q/width) (q/height))}))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsl 360 100.0 100.0 1.0)
  (initial-conditions))

(defn update-state [{:keys [triangles] :as state}]
  (if (> (count triangles) (Math/pow 2 13))
    (initial-conditions)
    ;; bias towards subdividing largest triangles
    (let [[above below] (cs/split-by
                         (fn [{:keys [depth]}] (< depth 12))
                         triangles)
          [to-divide remaining] (split-at 32 (shuffle above))]
      (assoc state :triangles
             (concat below
                     (mapcat subdivide to-divide)
                     remaining)))))

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
