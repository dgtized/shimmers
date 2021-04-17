(ns shimmers.sketches.decaying-foundations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.probability :as p]
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

(defn layer [y width height]
  (let [x-gap (tm/random (/ width 24) (/ width 12))]
    (for [x (range (- (tm/random width)) (q/width) width)]
      (geom/translate (brick (- width x-gap) height)
                      (gv/vec2 x y)))))

(defn wall [height]
  (let [y-gap (tm/random (/ height 16) (/ height 8))]
    (flatten
     (for [y (range (- (tm/random height)) (q/height) height)]
       (layer (+ y (/ y-gap 2))
              (rand-nth (map (partial * height) (range 1.1 3 0.5)))
              (- height y-gap))))))

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
  (doseq [{[x y] :p  [w h] :size :as rect} (wall 60)]
    (q/rect x y w h)
    (when (p/chance (/ y 1.5 (q/height)))
      (doseq [[p q] (hatches rect 0 (rand-nth [42 48 64 96]))]
        (apply q/line (-> (gl/line2 p q)
                          (geom/scale-size (tm/random 0.88 0.99))
                          geom/vertices))))))

(defn ^:export run-sketch []
  ;; 20210414
  (q/defsketch decaying-foundations
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
