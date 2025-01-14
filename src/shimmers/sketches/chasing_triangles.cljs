(ns shimmers.sketches.chasing-triangles
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; TODO: merge with triangle-mosiac
(defn triangle [[i j k] side]
  (let [hside (* side eq/SQRT3_2)
        x (+ i (* 0.5 j))]
    (if (zero? k)
      (gt/triangle2 (gv/vec2 (* (+ x 0.0) side) (* j hside))
                    (gv/vec2 (* (+ x 1.0) side) (* j hside))
                    (gv/vec2 (* (+ x 0.5) side) (* (inc j) hside)))
      (gt/triangle2 (gv/vec2 (* (inc x) side) (* j hside))
                    (gv/vec2 (* (+ (inc x) 0.5) side) (* (inc j) hside))
                    (gv/vec2 (* (- (inc x) 0.5) side) (* (inc j) hside))))))

(defn shapes [{p :p [width height] :size} side]
  (let [wn (/ width side)
        hside (* side eq/SQRT3_2)
        o (tm/+ p (gv/vec2 (* -2 side) (* -0.33 side)))]
    (apply concat
           (for [i (range wn)
                 j (range (/ height hside))
                 :let [tl (g/translate (triangle [i j 0] side) o)
                       tr (g/translate (triangle [i j 1] side) o)]]
             [tl tr]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:seed (dr/noise-seed)
   :shapes
   (for [s (shapes (cq/screen-rect 0.66) 60)]
     {:shape (poly-detect/inset-polygon s 2)
      :v (dr/random -0.15 0.15)
      :t0 0
      :t1 1})})

(defn shape-update [{:keys [shape v] :as s} t seed]
  (let [p (g/centroid shape)
        [x y] p
        pw (/ x (q/width))
        ph (/ y (q/height))
        vv (dr/noise-at-point-01 seed 0.0001 p)]
    (assoc s
           :t0 (mod (+ vv
                       (eq/unit-phase-sin 0.05 t
                                          (* 0.5 (eq/unit-sin (+ (* 0.5 pw t) ph v)))))
                    1.0)
           :t1 (eq/unit-phase-sin 0.1 t
                                  (* 0.5 (eq/unit-sin (- (+ (* 0.5 ph t) pw) v)))))))

(defn shapes-update [shapes t seed]
  (map (fn [shape] (shape-update shape t seed)) shapes))

(defn update-state [{:keys [seed] :as state}]
  (let [t (/ (q/millis) 1000)]
    (update state :shapes shapes-update t
            (tm/+ seed (tm/* (gv/vec2 0.1 0.1) (* 0.00001 t))))))

(comment
  (gu/arc-length-index (g/vertices (triangle/inscribed-equilateral {} 0))))

(defn vertices-between
  "Given a `shape`, return the sequence of vertices between `t0` and `t1`.

  Point order will always be in the same ordering as shape vertices. If `t1` <
  `t0` then they are swapped."
  [{:keys [shape t0 t1]}]
  (let [sv (g/vertices shape)
        vertices (conj (vec sv) (first sv))
        arc-idx (gu/arc-length-index vertices)
        [t0 t1] (if (< t1 t0) [t1 t0] [t0 t1])
        [p0 i0] (if (== 1.0 t0)
                  [(last vertices) (dec (count vertices))]
                  (gu/point-at* vertices arc-idx (peek arc-idx) t0 1))
        [p1 i1] (if (== 1.0 t1)
                  [(last vertices) (dec (count vertices))]
                  (gu/point-at* vertices arc-idx (peek arc-idx) t1 1))
        last-idx (if (< i0 i1) i1 (+ i1 (count vertices)))]
    (concat [p0]
            (if (= i0 i1)
              []
              (take (- last-idx i0) (drop i0 (cycle vertices))))
            [p1])))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (doseq [s shapes]
    (cq/draw-path (vertices-between s))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition chasing-triangles
  {:created-at "2025-01-13"
   :tags #{:genuary2025}
   :type :quil}
  (ctrl/mount page))
