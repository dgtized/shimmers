(ns shimmers.sketches.chasing-triangles
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]))

(defonce !defo (debug/state {}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes
   [{:shape (triangle/inscribed-equilateral (cq/rel-vec 0.5 0.5) (cq/rel-h 0.35) 0)
     :t0 0
     :t1 1}]})

(defn shape-update [s t]
  (assoc s
         :t0 (eq/unit-phase-sin 0.05 t (* 0.5 (eq/unit-sin (* 0.3 t))))
         :t1 (eq/unit-phase-sin 0.1 t (* 0.5 (eq/unit-sin (* 0.2 t))))))

(defn shapes-update [shapes t]
  (map (fn [shape] (shape-update shape t)) shapes))

(defn update-state [state]
  (let [t (/ (q/millis) 1000)]
    (update state :shapes shapes-update t)))

(comment
  (gu/arc-length-index (g/vertices (triangle/inscribed-equilateral {} 0))))

(defn arc-seq
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
  (reset! !defo shapes)
  (q/background 1.0)
  (doseq [s shapes]
    (cq/draw-path (arc-seq s))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [debug/display !defo]])

(sketch/definition chasing-triangles
  {:created-at "2025-01-13"
   :tags #{:genuary2025}
   :type :quil}
  (ctrl/mount page))
