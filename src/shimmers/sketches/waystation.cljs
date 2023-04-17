(ns shimmers.sketches.waystation
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

;; TODO: fix starting/stopping so it's proportional to mass or car count

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
  (let [n 7
        min-dist 0.06
        tracks (dr/density-range (max min-dist (/ 0.5 (inc n))) (* 2 (/ 1.0 (inc n))))
        tracks (if (> (last tracks) (- 1.0 min-dist))
                 (butlast tracks)
                 tracks)]
    {:t 0.0
     :tracks tracks
     :trains [{:track (dr/random-int (count tracks))
               :cars (dr/random-int 3 9)
               :pos (dr/random)
               :vel (dr/random 0.01 0.03)}]}))

(def max-accel 0.02)

(defn stopping-distance [vel]
  (let [dt 0.1] ;; FIXME
    (/ (- (eq/sqr vel)) (* dt -2 max-accel))))

(comment (stopping-distance 0.1))

(defn add-trains [tracks trains]
  (if (< (count trains) (count tracks))
    (let [diff (set/difference (set (range (count tracks)))
                               (set (map :track trains)))
          track (dr/rand-nth (vec diff))
          stopping-point (dr/gaussian 0.75 0.01)
          velocity (dr/random 0.03 0.12)
          cars (dr/random-int 3 15)
          train {:track track
                 :cars cars
                 :pos (- (+ (stopping-distance velocity) (dr/gaussian 1.0 0.05)))
                 :target (when (dr/chance 0.5) stopping-point)
                 :vel velocity}]
      (conj trains train))
    trains))

(defn move-train [dt {:keys [pos vel target cars] :as train}]
  (let [acc (if target
              (let [control 0.1
                    delta (- target pos)
                    acc (- (* control delta) (* (* 2 (Math/sqrt control)) vel))]
                (tm/clamp acc (- max-accel) (/ max-accel cars)))
              0)]
    (-> train
        (assoc :target (if (and target (<= (abs vel) 0.00001)
                                (dr/chance 0.008))
                         10.0
                         target))
        (update :vel + (* acc dt))
        (update :pos + (* vel dt)))))

(defn update-trains [trains tracks dt]
  (->> trains
       (mapv (partial move-train dt))
       (remove left-station?)
       (add-trains tracks)))

(defn update-state [{:keys [tracks] :as state}]
  (let [dt 0.1]
    (-> state
        (update :trains update-trains tracks dt)
        (update :t + dt))))

(defn draw-track [offset th]
  (q/stroke-weight 1.0)
  (q/line (cq/rel-vec 0 (- offset th)) (cq/rel-vec 1.0 (- offset th)))
  (q/line (cq/rel-vec 0 (+ offset th)) (cq/rel-vec 1.0 (+ offset th)))
  (q/stroke-weight 0.5)
  (doseq [x (range 0.01 1 0.018)]
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

    (q/stroke-weight 1.0)

    (doseq [{:keys [track] :as train} trains]
      (draw-train train (nth tracks track) track-height))))

(sketch/defquil waystation
  :created-at "2023-03-17"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
