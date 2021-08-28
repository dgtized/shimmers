(ns shimmers.sketches.grid-variations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn hashmark [t]
  (let [[l h] [0.2 0.8]
        offsets [0.4 0.6]]
    (map (fn [l]
           (-> l
               (geom/translate (gv/vec2 -0.5 -0.5))
               (geom/rotate t)))
         (apply concat (for [o offsets]
                         [(gl/line2 o l o h)
                          (gl/line2 l o h o)])))))

(defn perturb [pos radius]
  (gv/vec2 (p/confusion-disk pos radius)))

(defn xy-proportional [pos]
  (/ (* (:x pos) (:y pos))
     (* (q/width) (q/height))))

(defn x-proportional [pos]
  (/ (:x pos) (q/width)))

(defn y-proportional [pos]
  (/ (:y pos) (q/height)))

(defn invert [x]
  (- 1.0 x))

(defn sin-rate [rate]
  (/ (inc (Math/sin (* rate (q/frame-count)))) 2))

(defn chain-compose [fns]
  (fn [pos]
    (apply * ((apply juxt fns) pos))))

(defn gen-mode []
  (let [constants
        {(constantly 1) 3
         xy-proportional 1
         (comp invert xy-proportional) 1
         x-proportional 1
         (comp invert x-proportional) 1
         y-proportional 1
         (comp invert y-proportional) 1}]
    {:scalar
     (->> (rand-nth [(constantly 1.0)
                     (fn [_] (p/gaussian 1 0.1))
                     (fn [_] (p/gaussian 1 0.2))
                     (fn [_] (tm/map-interval (Math/sin (/ (q/frame-count) 100))
                                             [-1 1] [0.2 2.0]))])
          (conj [(p/weighted constants)])
          chain-compose)
     :rotation
     (->> (rand-nth [(constantly 1.0)
                     (fn [_] (p/gaussian 1 0.1))
                     (fn [_] (sin-rate 0.05))])
          (conj [(p/weighted constants)])
          chain-compose)}))

;; TODO: tween between modes by mixing from 0 to 1 for current and next
;; scalar/rotation functions.
(defn setup []
  (q/color-mode :hsl 1.0)
  {:modes [(gen-mode) (gen-mode)]
   :tween 0.0})

(defn update-state [state]
  (assoc state :tween
         (tm/map-interval (Math/sin (/ (q/frame-count) 400))
                          [-1 1] [0 1])))

(defn draw-mark [pos scale rotation]
  (doseq [{[p q] :points} (hashmark rotation)]
    (q/line (tm/+ pos (tm/* p scale))
            (tm/+ pos (tm/* q scale)))))

(defn animate-grid []
  (let [t (/ (q/frame-count) 100)]
    [(tm/map-interval (Math/sin t) [-1 1] [3 36])
     (tm/map-interval (Math/cos t) [-1 1] [4 24])]))

(defn draw [{:keys [modes tween]}]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (let [[{rot-a :rotation scalar-a :scalar}
         {rot-b :rotation scalar-b :scalar}] modes
        [I J] [11 13] ;; (animate-grid)
        area (* (q/height) (q/width))
        delta (tm/* (gv/vec2 (q/width) (q/height)) (gv/vec2 (/ 1 I) (/ 1 J)))
        scale (/ (Math/sqrt area) (Math/sqrt (* I J)))]
    (doseq [i (range I)]
      (doseq [j (range J)]
        (let [pos (tm/* (gv/vec2 (+ i 0.5) (+ j 0.5)) delta)]
          (draw-mark pos
                     (* scale (tm/mix* (scalar-a pos) (scalar-b pos) tween))
                     (* tm/TWO_PI (tm/mix* (rot-a pos) (rot-b pos) tween))))))))

(sketch/defquil grid-variations
  :created-at "2021-08-25"
  :size [600 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
