(ns shimmers.sketches.waystation
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.sequence :as cs]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

;; TODO: add slowing/stopping and then accelerating to leave

(let [length 0.07
      gap 0.005]
  (defn car-left [pos i]
    (- pos (* (inc i) length) (* gap i)))

  (defn car-right [pos i]
    (- pos (* i length) (* gap i))))

(defn left-station? [{:keys [pos cars]}]
  (> (car-left pos cars) 1.0))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n 7]
    {:t 0.0
     :tracks (cs/midsection (tm/norm-range (inc n)))
     :trains [{:track 0 :cars 3 :pos 0.9 :vel 0.01}]}))

(defn add-trains [trains tracks]
  (if (< (count trains) (count tracks))
    (let [diff (set/difference (set (range (count tracks)))
                               (set (map :track trains)))
          track (dr/rand-nth (vec diff))
          train {:track track
                 :cars (dr/random-int 3 15)
                 :pos (dr/random -0.5 -0.1)
                 :vel (dr/random 0.01 0.12)}]
      (conj trains train))
    trains))

(defn move-train [dt {:keys [vel] :as train}]
  (update train :pos + (* vel dt)))

(defn update-state [{:keys [tracks] :as state}]
  (let [dt 0.1]
    (-> state
        (update :trains add-trains tracks)
        (update :trains (partial mapv (partial move-train dt)))
        (update :t + dt)
        (update :trains (partial remove left-station?)))))

(defn draw-track [offset th]
  (q/line (cq/rel-vec 0 (- offset th)) (cq/rel-vec 1.0 (- offset th)))
  (q/line (cq/rel-vec 0 (+ offset th)) (cq/rel-vec 1.0 (+ offset th)))
  (doseq [x (range 0.01 1 0.015)]
    (q/line (cq/rel-vec x (- offset (* th 1.2)))
            (cq/rel-vec x (+ offset (* th 1.2))))))

(defn draw-train [{:keys [pos cars]} track track-height]
  (let [th (* track-height 0.85)]
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
  (let [track-height 0.02]
    (doseq [track tracks]
      (draw-track track track-height))

    (doseq [{:keys [track] :as train} trains]
      (draw-train train (nth tracks track) track-height))))

(sketch/defquil waystation
  :created-at "2023-05-17"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
