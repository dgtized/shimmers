(ns shimmers.sketches.grid-variations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.transition-interval :as transition]
            [shimmers.math.equations :as eq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
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

(defn centered-x [pos]
  (eq/gaussian 1.0 0.5 0.25 (x-proportional pos)))

(defn centered-y [pos]
  (eq/gaussian 1.0 0.5 0.25 (y-proportional pos)))

(defn centered-xy [pos]
  (* (centered-x pos) (centered-y pos)))

(defn invert [x]
  (- 1.0 x))

(defn sin-rate [rate]
  (/ (inc (Math/sin (* rate (q/frame-count)))) 2))

(defn chain-compose [fns]
  (fn [pos]
    (apply * ((apply juxt fns) pos))))

(def constant-factors
  {(constantly 1) 3
   xy-proportional 1
   (comp invert xy-proportional) 1
   x-proportional 1
   (comp invert x-proportional) 1
   y-proportional 1
   (comp invert y-proportional) 1
   centered-xy 1
   (comp invert centered-xy) 1
   centered-x 1
   (comp invert centered-x) 1
   centered-y 1
   (comp invert centered-y) 1})

(defn option-from [options]
  (chain-compose [(p/weighted options)
                  (p/weighted constant-factors)]))

(defn gen-mode []
  {:position
   (p/weighted {(constantly (gv/vec2)) 3
                (let [radius (tm/random 1.0 4.0)]
                  (fn [_] (perturb [0 0] radius))) 1
                (let [radius (tm/random 3.0 6.0)
                      speed (p/weighted {5 1 10 2 15 3 20 2})]
                  (fn [_] (v/polar radius (/ (q/frame-count) speed)))) 1})
   :scalar
   (option-from {(constantly 1.0) 1
                 (fn [_] (p/gaussian 1 0.1)) 1
                 (fn [_] (p/gaussian 1 0.2)) 1
                 (fn [_] (tm/map-interval (Math/sin (/ (q/frame-count) 100))
                                         [-1 1] [0.2 2.0])) 1})
   :rotation
   (option-from {(constantly 0.0) 1
                 (fn [_] (p/gaussian 1 0.1)) 1
                 (let [mag (* (rand-nth [-1 1])
                              (rand-nth [100 150 200 250]))]
                   (fn [_] (/ (q/frame-count) mag))) 1
                 (fn [_] (sin-rate 0.05)) 1})})

(defn animate-grid []
  (let [t (/ (q/frame-count) 100)]
    (gv/vec2 [(tm/map-interval (Math/sin t) [-1 1] [3 36])
              (tm/map-interval (Math/cos t) [-1 1] [4 24])])))

(defn gen-grid []
  (p/weighted {(constantly (gv/vec2 11 13)) 1
               (constantly (gv/vec2 13 17)) 2
               (constantly (gv/vec2 17 23)) 3
               (constantly (gv/vec2 23 29)) 2
               animate-grid 2}))

;; Add state for shape+deformations, stroke-weight, color, and opacity.
(defn setup []
  (q/color-mode :hsl 1.0)
  {:modes [(gen-mode) (gen-mode)]
   :grid [(gen-grid) (gen-grid)]
   :transition (transition/after 0 400)
   :tween 0.0})

(defn update-state [{:keys [transition] :as state}]
  (let [fc (q/frame-count)]
    (if (transition/complete? transition fc)
      (assoc state
             :modes [(last (:modes state)) (gen-mode)]
             :grid (let [last-grid (last (:grid state))]
                     [last-grid (if (p/chance 0.3)
                                  (gen-grid)
                                  last-grid)])
             :transition (transition/after fc (* 100 (rand-nth [3 4 5 6 7 7 8 9 10])))
             :tween 0.0)
      (assoc state :tween (tm/smoothstep* 0.3 0.7 (transition/percent transition fc))))))

(defn draw-mark [pos scale rotation]
  (doseq [{[p q] :points} (hashmark rotation)]
    (q/line (tm/+ pos (tm/* p scale))
            (tm/+ pos (tm/* q scale)))))

(defn draw [{:keys [modes grid tween]}]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (let [[{rot-a :rotation scalar-a :scalar pos-a :position}
         {rot-b :rotation scalar-b :scalar pos-b :position}] modes
        [g1 g2] grid
        [I J] (tm/mix (g1) (g2) tween)
        area (* (q/height) (q/width))
        delta (tm/* (gv/vec2 (q/width) (q/height)) (gv/vec2 (/ 1 I) (/ 1 J)))
        scale (/ (Math/sqrt area) (Math/sqrt (* I J)))]
    (doseq [i (range I)]
      (doseq [j (range J)]
        (let [pos (tm/* (gv/vec2 (+ i 0.5) (+ j 0.5)) delta)]
          (draw-mark (tm/+ pos (tm/mix (pos-a pos) (pos-b pos) tween))
                     (* scale (tm/mix* (scalar-a pos) (scalar-b pos) tween))
                     (* tm/TWO_PI (tm/mix* (rot-a pos) (rot-b pos) tween))))))))

(sketch/defquil grid-variations
  :created-at "2021-08-25"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
