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

(defn make-triangle [a b c & {:keys [color depth] :or {depth 0}}]
  (assoc (gt/triangle2 a b c)
         :color color
         :depth depth))

(defn drift [[h s l a]]
  (if (< (rand) 0.05)
    [(mod (+ h 90) 360)
     (+ (* 0.75 (q/random-gaussian)) s)
     (+ (* 0.1 (q/random-gaussian)) l)
     (* 0.5 a)]
    [(mod (+ (* 8 (q/random-gaussian)) h) 360) s (+ 1 l) a]))

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

(defn dividable? [{:keys [depth final]}]
  (and (not final) (< depth 16)))

(defn subdivide [{:keys [color depth] :as s}]
  (for [child (subdivide-triangle s)
        :let [t (assoc child
                       :final (and (> depth 1) (< (rand) 0.3))
                       :depth depth)]]
    (if (and color (< (rand) 0.90))
      (assoc t :color (drift color))
      t)))

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
    [(make-triangle a b c :color [180 60 55 0.8])
     (make-triangle c d b :color [0 55 45 0.8])]))

(defn initial-conditions []
  (let [shape-fn (rand-nth [one-triangle split-rectangle])]
    {:triangles (shape-fn (q/width) (q/height))}))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsl 360 100.0 100.0 1.0)
  (initial-conditions))

(defn subdivide-batch [{:keys [triangles] :as state}]
  (if (> (count triangles) 10000)
    [true state]
    (let [[above below] (cs/split-by dividable? triangles)
          [to-divide remaining] (split-at 16 (shuffle above))]
      [false
       (assoc state :triangles
              (concat below
                      (mapcat subdivide to-divide)
                      remaining))])))

(defn update-state [state]
  (quil/if-steady-state
   state 5
   initial-conditions
   subdivide-batch))

(defn draw-triangle [a b c]
  (q/triangle (:x a) (:y a) (:x b) (:y b) (:x c) (:y c)))

(defn draw [{:keys [triangles]}]
  (q/background 255)
  (q/stroke-weight 0.1)
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
