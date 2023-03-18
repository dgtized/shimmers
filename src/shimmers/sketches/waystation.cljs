(ns shimmers.sketches.waystation
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.sequence :as cs]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

(let [length 0.07
      gap 0.005]
  (defn car-left [pos i]
    (- pos (* (inc i) length) (* gap i)))

  (defn car-right [pos i]
    (- pos (* i length) (* gap i))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n 2]
    {:t 0.0
     :tracks (cs/midsection (tm/norm-range (inc n)))
     :trains [{:track 0 :cars 3 :pos 0.9 :vel 0.01}]}))

(defn move-train [dt {:keys [vel] :as train}]
  (update train :pos + (* vel dt)))

(defn update-state [state]
  (let [dt 0.1]
    (-> state
        (update :trains (partial mapv (partial move-train dt)))
        (update :t + dt))))

(defn draw-track [offset]
  (let [th 0.02]
    (q/line (cq/rel-vec 0 (- offset th)) (cq/rel-vec 1.0 (- offset th)))
    (q/line (cq/rel-vec 0 (+ offset th)) (cq/rel-vec 1.0 (+ offset th)))
    (doseq [x (range 0.01 1 0.015)]
      (q/line (cq/rel-vec x (- offset (* th 1.2))) (cq/rel-vec x (+ offset (* th 1.2)))))))

(defn draw-train [{:keys [pos cars]} track]
  (let [th 0.017]
    (doseq [i (range (inc cars))]
      (let [left (car-left pos i)
            right (car-right pos i)]
        (qdg/draw (rect/rect (cq/rel-vec left (- track th))
                             (cq/rel-vec right (+ track th))))
        (when (pos? i)
          (q/line (cq/rel-vec right track)
                  (cq/rel-vec (car-left pos (dec i)) track)))))))

(defn draw [{:keys [tracks trains]}]
  (q/background 1.0)
  (doseq [track tracks]
    (draw-track track))

  (doseq [{:keys [track] :as train} trains]
    (draw-train train (nth tracks track))))

(sketch/defquil waystation
  :created-at "2023-05-17"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
