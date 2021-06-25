(ns shimmers.sketches.decaying-foundations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/no-loop)
  (q/rect-mode :corner)
  (q/color-mode :hsl 1.0)
  {})

(defn brick [w h]
  (rect/rect 0 0 w h))

(defn layer [y x-off width height]
  (let [x-gap (tm/random (/ width 24) (/ width 12))]
    (for [x (range (* -0.33 width x-off) (q/width) width)]
      (geom/translate (brick (- width x-gap) height)
                      (gv/vec2 x y)))))

(defn wall [brick-height]
  (let [screen-height (q/height)
        y-gap (tm/random (/ brick-height 16) (/ brick-height 8))]
    (flatten
     (for [y (range (- (tm/random brick-height)) screen-height brick-height)
           :let [py (/ y screen-height)]]
       (->> (layer (+ y (/ y-gap 2))
                   (inc (mod (int (/ y brick-height)) 3))
                   (* brick-height (p/weighted {1.2 (* 1.2 py)
                                                1.5 (* 1.0 py)
                                                1.8 (- 2.0 py)
                                                2.1 (- 1.5 py)
                                                2.4 (- 1.0 py)}))
                   (- brick-height y-gap))
            shuffle
            (drop (int (* py (tm/random 2.9)))))))))

(defn update-state [state]
  state)

(defn hatches [rect skip n]
  (let [spacing (/ (geom/circumference rect) n)
        points (geom/sample-uniform rect spacing false)]
    (loop [points (drop skip points)
           lines []]
      (if (<= (count points) skip)
        lines
        (let [p (first points)
              q (last points)]
          (recur (rest (butlast points)) (conj lines [p q])))))))

(defn draw [_]
  (q/background 0.9 1.0)
  (doseq [{[x y] :p  [w h] :size :as rect} (wall 45)
          :let [py (/ y (q/height))]]
    (q/fill (if (< py 0.55) (tm/random -0.05 0.05) (tm/random 0.35 0.45))
            (* (- 1.8 py) (tm/random 0.3 0.6))
            (* (- 1.2 py) (tm/random 0.2 0.4))
            (+ (Math/pow (- py 0.45) 2) 0.25))
    (q/stroke-weight 1.0)
    (q/rect x y w h)
    (when (p/chance (/ py 1.5))
      (doseq [[p q] (hatches rect 0 (rand-nth [42 48 64 84]))]
        (q/stroke-weight (tm/random 0.5 0.8))
        (apply q/line (-> (gl/line2 (p/jitter-x p 0.66) (p/jitter-x q 0.66))
                          (geom/scale-size (tm/random 0.88 0.96))
                          geom/vertices))))))

(sketch/defquil decaying-foundations
  :created-at "2021-04-14"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
