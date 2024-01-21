(ns shimmers.sketches.alignment-perturbations
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]))

(def ^:constant SQRT3_2 (/ (Math/sqrt 3) 2))

(defn triangle [[i j k] side]
  (let [hside (* side (/ (Math/sqrt 3) 2))
        x (+ i (* 0.5 j))]
    (if (zero? k)
      (gt/triangle2 (gv/vec2 (* (+ x 0.0) side) (* j hside))
                    (gv/vec2 (* (+ x 1.0) side) (* j hside))
                    (gv/vec2 (* (+ x 0.5) side) (* (inc j) hside)))
      (gt/triangle2 (gv/vec2 (* (inc x) side) (* j hside))
                    (gv/vec2 (* (+ (inc x) 0.5) side) (* (inc j) hside))
                    (gv/vec2 (* (- (inc x) 0.5) side) (* (inc j) hside))))))

(defn grid [{p :p [width height] :size} side]
  (let [wn (Math/ceil (/ width side))
        hn (Math/ceil (/ height (* SQRT3_2 side)))]
    (for [i (range wn)
          j (range hn)
          k (range 2)]
      (-> (triangle [(- i (Math/ceil (* j 0.5))) j k] side)
          (g/translate  p)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 1.0)
  (let [side (/ (q/width) 20)
        bounds (rect/rect 20 (* 20 SQRT3_2)
                          (Math/floor (* 19 side))
                          (Math/floor (* 16 side SQRT3_2)))]
    (q/no-fill)
    (qdg/draw bounds)
    (q/fill 0.0 0.5 0.6 0.1)
    (doseq [t (grid bounds side)]
      (qdg/draw t))))

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
