(ns shimmers.sketches.decaying-foundations
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/no-loop)
  (q/rect-mode :corner)
  (q/color-mode :hsl 1.0)
  {})

(defn brick [w h]
  (rect/rect 0 0 w h))

(defn layer [y x-off width height]
  (let [x-gap (dr/random (/ width 24) (/ width 12))]
    (for [x (range (* -0.33 width x-off) (q/width) width)]
      (g/translate (brick (- width x-gap) height)
                   (gv/vec2 x y)))))

(defn wall [brick-height]
  (let [screen-height (q/height)
        y-gap (dr/random (/ brick-height 16) (/ brick-height 8))]
    (flatten
     (for [y (range (- (dr/random brick-height)) screen-height brick-height)
           :let [py (/ y screen-height)]]
       (->> (layer (+ y (/ y-gap 2))
                   (inc (mod (int (/ y brick-height)) 3))
                   (* brick-height (dr/weighted {1.2 (* 1.2 py)
                                                 1.5 (* 1.0 py)
                                                 1.8 (- 2.0 py)
                                                 2.1 (- 1.5 py)
                                                 2.4 (- 1.0 py)}))
                   (- brick-height y-gap))
            dr/shuffle
            (drop (int (* py (dr/random 2.9)))))))))

(defn update-state [state]
  state)

(defn hatches [rect skip n]
  (let [spacing (/ (g/circumference rect) n)
        points (g/sample-uniform rect spacing false)]
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
    (q/fill (if (< py 0.55) (dr/random -0.05 0.05) (dr/random 0.35 0.45))
            (* (- 1.8 py) (dr/random 0.3 0.6))
            (* (- 1.2 py) (dr/random 0.2 0.4))
            (+ (Math/pow (- py 0.45) 2) 0.25))
    (q/stroke-weight 1.0)
    (q/rect x y w h)
    (when (dr/chance (/ py 1.5))
      (doseq [[p q] (hatches rect 0 (dr/rand-nth [42 48 64 84]))]
        (q/stroke-weight (dr/random 0.5 0.8))
        (apply q/line (-> (gl/line2 (dr/jitter-x p 0.66) (dr/jitter-x q 0.66))
                          (g/scale-size (dr/random 0.88 0.96))
                          g/vertices))))))

(sketch/defquil decaying-foundations
  :created-at "2021-04-14"
  :tags #{:static :deterministic}
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
