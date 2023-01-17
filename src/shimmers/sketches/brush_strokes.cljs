(ns shimmers.sketches.brush-strokes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn generate-brush [line]
  (let [len (tm/mag line)]
    {:point (g/centroid line)
     :line line
     :facing (gv/vec2 1 0)
     :vel (gv/vec2 0 0)
     :angle-vel 0.0
     :bristles
     (vec
      (for [[a b] (partition 2 (dr/gaussian-range 0.12 0.03 true))]
        (let [c (+ a (* 0.5 (- b a)))]
          (triangle/inscribed-equilateral
           (gc/circle (g/point-at line c) (* 1.5 len (- b a)))
           (dr/random eq/TAU)))))}))

(defn translate-brush [brush p]
  (-> brush
      (update :point g/translate p)
      (update :line g/translate p)
      (update :bristles (partial mapv (fn [b] (g/translate b p))))))

(defn rotate-brush [{:keys [point] :as brush} t]
  (-> brush
      (update :facing g/rotate t)
      (update :line geometry/rotate-around point t)
      (update :bristles (partial mapv (fn [b] (geometry/rotate-around b point t))))))

(defn rotate-bristles [brush t]
  (-> brush
      (update :bristles (partial mapv (fn [b] (geometry/rotate-around-centroid b t))))))

;; see also https://gamedev.stackexchange.com/questions/1885/target-tracking-when-to-accelerate-and-decelerate-a-rotating-turret
(defn follow [{:keys [point facing vel angle-vel] :as brush} target dt]
  (let [dir (tm/- target point)
        dv (tm/* dir (/ (* 0.2 dt) (tm/mag dir)))
        vel' (tm/* (tm/+ vel dv) 0.99)
        pos' (tm/+ point (tm/* vel dt))
        delta-angle (let [delta (- (g/heading dir) (g/heading facing))]
                      (cond (< delta (- Math/PI)) (+ delta eq/TAU)
                            (> delta Math/PI) (- delta eq/TAU)
                            :else delta))
        angle-control 0.0001
        angle-acc (* dt (- (* angle-control delta-angle)
                           (* (* 2 (Math/sqrt angle-control)) angle-vel)))
        angle-vel' (+ angle-vel angle-acc)]
    (-> brush
        (translate-brush (tm/- pos' point))
        (rotate-brush angle-vel)
        (rotate-bristles (* angle-vel (/ 1 3)))
        (assoc :vel vel'
               :angle-vel angle-vel'))))

(defn generate-scribble []
  (vec (reverse (into [(cq/rel-vec 0.05 0.5)
                       (cq/rel-vec 0.85 0.5)]
                      (repeatedly 14 #(cq/rel-vec (dr/random 0.15 0.85)
                                                  (dr/random 0.15 0.85)))))))

(defn spiral [center dr dtheta steps]
  (for [theta (range 0 (* steps dtheta) dtheta)]
    (v/+polar center (* dr (/ theta tm/TWO_PI)) theta)))

(defn gen-spiral []
  (let [path (spiral (cq/rel-vec 0.5 0.5)
                     (cq/rel-h (dr/random 0.07 0.11))
                     (dr/random 0.35 0.45)
                     (dr/random-int 64 82))]
    (vec (case (dr/weighted {:connect 1
                             :outside 1
                             :connect-outside-in 1
                             :outside-in 1})
           :outside-in path
           :connect-outside-in (concat path (take 1 path))
           :connect (cons (first path) (reverse path))
           :outside (reverse path)))))

(defn generate-spiral-pair []
  (vec (reverse (concat (spiral (cq/rel-vec 0.25 0.5) (cq/rel-h 0.08) 0.4 50)
                        (spiral (cq/rel-vec 0.7 0.5) (cq/rel-h 0.08) 0.5 60)))))

(defn gen-circle []
  (let [points (g/vertices (gc/circle (cq/rel-vec (dr/rand-nth [0.35 0.4 0.5 0.6 0.65])
                                                  (dr/rand-nth [0.45 0.5 0.55]))
                                      (cq/rel-h (dr/random 0.25 0.35)))
                           24)
        path (cs/rotate (dr/random-int (count points)) points)]
    ((if (dr/chance 0.5) reverse identity)
     (concat path (take 1 path)))))

(defn gen-rectangle []
  (let [width (q/width)
        height (q/height)
        w (dr/random 0.3 0.6)
        h (dr/random 0.3 0.6)
        points (g/vertices
                (rect/rect (* width (dr/random 0.1 (- 0.9 w)))
                           (* height (dr/random 0.1 (- 0.9 h)))
                           (* w width) (* h height)))
        path (cs/rotate (dr/random-int (count points)) points)]
    ((if (dr/chance 0.5) reverse identity)
     (concat path (take 1 path)))))

(defn gen-scene []
  (->> (fn [] ((dr/weighted {gen-circle 1
                            gen-spiral 1
                            gen-rectangle 1})))
       (repeatedly (dr/weighted {2 2
                                 3 1}))
       (apply concat)
       vec))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [scene ((dr/weighted {generate-scribble 1
                             gen-spiral 1
                             generate-spiral-pair 1
                             gen-circle 1
                             gen-rectangle 1
                             gen-scene 4}))
        path (->> scene
                  (drop (if (or (dr/chance 0.5) (< (count scene) 15))
                          0
                          (dr/random-int 10)))
                  vec)
        next-pt (peek path)
        h (cq/rel-vec 0.0 0.05)]
    {:t 0
     :path (pop path)
     :brush (generate-brush
             (gl/line2 (tm/- next-pt h) (tm/+ next-pt h)))}))

(defn update-state [{:keys [brush path] :as state}]
  (let [dt 0.25
        next-pt (peek path)]
    (if next-pt
      (-> (if (< (g/dist next-pt (:point brush)) (cq/rel-h 0.03))
            (update state :path pop)
            state)
          (update :t + dt)
          (update :brush follow next-pt dt))
      state)))

(defn draw [{:keys [brush path]}]
  (when (seq path)
    (q/no-stroke)
    (q/fill 0.0 0.05)
    (doseq [hair (:bristles brush)]
      (cq/draw-polygon hair))
    #_(q/stroke 0.0 1.0)
    #_(doseq [p path]
        (apply q/point p))))

(sketch/defquil brush-strokes
  :created-at "2023-01-15"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
