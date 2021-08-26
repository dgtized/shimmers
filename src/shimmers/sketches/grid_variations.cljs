(ns shimmers.sketches.grid-variations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.math.probability :as p]))

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

(defn mag-proportional [area pos]
  (/ (tm/mag-squared pos) area))

(defn gaussian [sd area pos]
  (p/gaussian (mag-proportional area pos) (sd pos)))

(defn sin-rate [rate area pos]
  (tm/map-interval (Math/sin (* rate (q/frame-count))) [-1 1]
                   [0 (* tm/TWO_PI (mag-proportional area pos))]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:scalar (rand-nth [(constantly 1)
                      (fn [_] (p/gaussian 1 0.1))
                      (fn [_] (tm/map-interval (Math/sin (/ (q/frame-count) 100))
                                              [-1 1] [0.2 2.0]))])
   :rotation (rand-nth [mag-proportional
                        (partial gaussian (constantly 0.1))
                        (partial sin-rate 0.05)])})

(defn update-state [state]
  state)

(defn draw-mark [pos scale rotation]
  (doseq [{[p q] :points} (hashmark rotation)]
    (q/line (tm/+ pos (tm/* p scale))
            (tm/+ pos (tm/* q scale)))))

(defn animate-grid []
  (let [t (/ (q/frame-count) 100)]
    [(tm/map-interval (Math/sin t) [-1 1] [3 36])
     (tm/map-interval (Math/cos t) [-1 1] [4 24])]))

(defn draw [{:keys [rotation scalar]}]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (let [[I J] [11 13] ;; (animate-grid)
        area (* (q/height) (q/width))
        delta (tm/* (gv/vec2 (q/width) (q/height)) (gv/vec2 (/ 1 I) (/ 1 J)))
        scale (/ (Math/sqrt area) (Math/sqrt (* I J)))]
    (doseq [i (range I)]
      (doseq [j (range J)]
        (let [pos (tm/* (gv/vec2 (+ i 0.5) (+ j 0.5)) delta)
              rotation (rotation area pos)]
          (draw-mark pos (* scale (scalar pos)) rotation))))))

(sketch/defquil grid-variations
  :created-at "2021-08-25"
  :size [600 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
