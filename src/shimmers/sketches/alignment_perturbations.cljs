(ns shimmers.sketches.alignment-perturbations
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn triangle
  [[i j k] side
   [[bias-i-a bias-i-b bias-i-c]
    [bias2-a bias2-b bias2-c]]]
  (let [hside (* side eq/SQRT3_2)
        x (+ i (* 0.5 j))]
    (if (zero? k)
      (gt/triangle2 (gv/vec2 (* (+ x 0.0 bias-i-a) side) (* j hside))
                    (gv/vec2 (* (+ x 1.0 bias-i-b) side) (* j hside))
                    (gv/vec2 (* (+ x 0.5 bias-i-c) side)
                             (* (+ (inc j) ) hside)))
      (gt/triangle2 (gv/vec2 (* (+ (inc x) 0.0 bias2-a) side) (* j hside))
                    (gv/vec2 (* (+ (inc x)  0.5 bias2-b) side) (* (inc j) hside))
                    (gv/vec2 (* (+ (inc x) -0.5 bias2-c) side) (* (inc j) hside))))))

(defn grid [{p :p [width height] :size} side bias]
  (let [wn (math/ceil (/ width side))
        hn (math/ceil (/ height (* eq/SQRT3_2 side)))]
    (for [i (range wn)
          j (range hn)
          k (range 2)]
      (-> (triangle [(- i (math/ceil (* j 0.5))) j k] side bias)
          (g/translate  p)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 1.0)
  (let [t (/ (q/millis) 1000.0)
        side (/ (q/width) 20)
        bounds (rect/rect 20 (* 20 eq/SQRT3_2)
                          (math/floor (* 19 side))
                          (math/floor (* 16 side eq/SQRT3_2)))
        bias [[(math/sin (+ 0.8 (* t 0.5) (math/sin (+ 0.5 (* 0.5 t)))))
               (math/sin (+ 0.9 (* t 0.3) (math/sin (+ 0.1 (* 0.4 t)))))
               (math/sin (+ 0.1 (* t 0.4) (math/sin (+ 0.1 (* 0.2 t)))))]
              [(math/sin (+ 0.3 (* t 0.6) (math/sin (+ 0.5 (* 0.5 t)))))
               (math/sin (+ 0.3 (* t 0.2) (math/sin (+ 0.1 (* 0.4 t)))))
               (math/sin (+ 0.1 (* t 0.7) (math/sin (+ 0.1 (* 0.2 t)))))]]
        d-point (tm/+ (cq/rel-vec 0.5 0.5) (v/polar (tm/mag (cq/rel-vec 0.2 0.2)) (* 0.7 t)))]
    (q/no-fill)
    (qdg/draw bounds)
    (q/fill 0.6 0.5 0.5 0.1)
    (doseq [triangle (grid bounds side bias)]
      (let [c (g/centroid triangle)
            d (tm/+ (tm/* (gv/vec2 1.0 1.0)
                          (* 0.1 (g/dist c d-point)))
                    (gv/vec2 (* -1.5 side) (* -1 side)))]
        (qdg/draw (g/translate triangle d))))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition alignment-perturbations
  {:created-at "2024-01-21"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
