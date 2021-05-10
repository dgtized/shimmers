(ns shimmers.common.quil
  (:require [quil.core :as q :include-macros true]
            [thi.ng.geom.vector :as gv]))

(defn rel-h [p]
  (* (q/height) p))

(defn rel-w [p]
  (* (q/width) p))

(defn rel-pos
  ([[w h]] (rel-pos w h))
  ([w h] [(rel-w w) (rel-h h)]))

(defn rel-vec
  ([[w h]] (rel-vec w h))
  ([w h] (gv/vec2 (rel-pos w h))))

(defn draw-shape [vertices]
  (q/begin-shape)
  (doseq [v vertices]
    (apply q/vertex v))
  (q/end-shape :close))

(defn draw-curve-shape [vertices]
  (q/begin-shape)
  (doseq [v vertices]
    (apply q/curve-vertex v))
  (q/end-shape :close))

(defn color-if
  "Apply `set-color-fn` with `color` if color is a sequence."
  [set-color-fn color]
  (when (seq color) (apply set-color-fn color)))

(defn draw-triangle [[ax ay] [bx by] [cx cy]]
  (q/triangle ax ay bx by cx cy))

(defn lerp-line [[x y] [x' y'] amt]
  (q/line x y (q/lerp x x' amt) (q/lerp y y' amt)))

(defn if-steady-state
  "Sketch update helper for restarting after a sketch reaches steady state.

  Use `update-state` to update `state`, however if it also returns done?, and
  then after `duration` use `restart-state` to recreate the sketch."
  [state duration restart-state update-state]
  (let [fc (q/frame-count)
        diff (- fc (get state :completed-frame fc))]
    (if (> (/ diff (q/current-frame-rate)) duration)
      (restart-state)
      (let [[done? new-state] (update-state state)]
        (if (and done? (nil? (:completed-frame state)))
          (assoc new-state :completed-frame fc)
          new-state)))))

(defn normal-material []
  (.normalMaterial (q/current-graphics)))
