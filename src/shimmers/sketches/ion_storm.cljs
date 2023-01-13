(ns shimmers.sketches.ion-storm
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 10)
  {:t 0
   :storms []})

(defn random-circle [r-param max-r]
  (let [R (min (q/height) (q/width))
        r (max (min r-param max-r) 0.0)
        radius (* R r)
        hr (* radius 0.5)]
    (gc/circle (gv/vec2 (dr/random hr (- (q/width) hr))
                        (dr/random hr (- (q/height) hr)))
               radius)))

(defn flash-storm [t]
  {:region (random-circle (dr/gaussian 0.15 0.05) 0.8)
   :vel (let [s (min (q/width) (q/height))]
          (dr/randvec2 (dr/gaussian (/ s 30) (/ s 90))))
   :t0 t
   :t1 (+ t (dr/random 0.5 1.5))})

(defn update-state [{:keys [t] :as state}]
  (-> state
      (update :storms
              (fn [storms]
                (let [storms' (->> storms
                                   (remove (fn [{:keys [t1]}] (>= t t1)))
                                   (map (fn [{:keys [vel] :as storm}]
                                          (update storm :region g/translate vel))))]
                  (if (dr/chance 0.15)
                    (conj storms' (flash-storm t))
                    storms'))))
      (update :t + (dr/random 0.1))))

(defn draw [{:keys [storms]}]
  (q/background 1.0 0.1)
  (q/ellipse-mode :radius)
  (q/stroke 0.0)
  (q/stroke-weight 3.0)

  (q/fill 0.0 0.25)
  (dotimes [_ 3]
    (cq/circle (random-circle (dr/gaussian 0.3 0.1) 0.5)))

  (q/fill 1.0 0.2)
  (dotimes [_ 6]
    (let [c (random-circle (dr/gaussian 0.05 0.1) 0.3)]
      (cq/draw-triangle (:points (triangle/inscribed-equilateral c (dr/random eq/TAU))))))

  (q/fill 0.6 0.1 0.9 0.08)
  (q/no-stroke)
  (doseq [{:keys [region]} storms]
    (let [{:keys [p r]} region]
      (dotimes [_ 48]
        (let [radius (* (max (min (dr/gaussian 0.2 0.1) 0.95) 0.0) r)
              c (rp/inside-circle (gc/circle p (- r radius)) dr/random)]
          (cq/draw-triangle
           (:points (triangle/inscribed-equilateral
                     (gc/circle c radius)
                     (dr/random eq/TAU)))))))))

(sketch/defquil ion-storm
  :created-at "2023-01-13"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
