(ns shimmers.sketches.triangulating-subdivisions
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.core :as geom]))

;; Ideas:
;; Color each triangle and then shade the subdivisions by the parent somehow?

(defrecord Line [a b])

(defn triangle->lines [t]
  (map (fn [[a b]] (->Line a b)) (geom/edges t)))

(defn subdivide-line [{:keys [a b]}]
  (v/add b (v/scale (v/sub a b) (q/random 0.25 0.75))))

(defn subdivide-triangle [t]
  (let [[a b c] (shuffle (:points t))
        approach (rand-nth [:midpoint :midpoint :inset :inset :centroid])]
    (case approach
      :midpoint
      (let [m (subdivide-line (->Line a b))]
        [(gt/triangle2 a m c)
         (gt/triangle2 b m c)])
      :centroid
      (let [inner (geom/random-point-inside t)]
        [(gt/triangle2 a b inner)
         (gt/triangle2 b c inner)
         (gt/triangle2 c a inner)])
      :inset
      (let [mab (subdivide-line (->Line a b))
            mbc (subdivide-line (->Line b c))
            mca (subdivide-line (->Line c a))]
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
  (doseq [line (mapcat triangle->lines triangles)]
    (q/line (:a line) (:b line))))

(defn ^:export run-sketch []
  (q/defsketch triangulating-subdivisions
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
