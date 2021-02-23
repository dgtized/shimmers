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

;; Ideas:
;; Color each triangle and then shade the subdivisions by the parent somehow?

(defn triangle->lines [t]
  (map (fn [[a b]] (gl/line2 a b)) (geom/edges t)))

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

(defn initial-conditions []
  (let [top (v/vec2 (* (q/random 0.1 0.9) (q/width)) (* 0.1 (q/height)))
        left (v/vec2 (* 0.1 (q/width)) (* 0.9 (q/height)))
        right (v/vec2 (* 0.9 (q/width)) (* 0.9 (q/height)))]
    {:triangles [(gt/triangle2 top left right)]}))

(defn setup []
  (q/frame-rate 10)
  (initial-conditions))

(defn area [t]
  (* 0.5 (geom/height t) (geom/width t)))

(defn update-state [{:keys [triangles] :as state}]
  (if (> (count triangles) 200)
    (initial-conditions)
    ;; bias towards subdividing largest triangles
    (let [ordered (sort-by area triangles)
          cutoff (int (* 0.33 (count triangles)))
          [s & r] (shuffle (drop cutoff ordered))]
      (assoc state :triangles (into (take cutoff ordered)
                                    (into (subdivide-triangle s) r))))))

(defn draw [{:keys [triangles]}]
  (q/background 255)
  (q/stroke-weight 0.05)
  (doseq [{[p q] :points} (mapcat triangle->lines triangles)]
    (q/line p q)))

(defn ^:export run-sketch []
  (q/defsketch triangulating-subdivisions
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
