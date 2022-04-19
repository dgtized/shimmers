(ns shimmers.common.quil
  (:require
   [quil.core :as q :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
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

(defn screen-rect
  "A rectangle proportional to the size of the current quil canvas."
  ([] (screen-rect 1.0))
  ([scale]
   (-> (rect/rect 0 0 (q/width) (q/height))
       (g/scale-size scale))))

(defn mouse-position []
  (gv/vec2 (q/mouse-x) (q/mouse-y)))

(defn mouse-last-position-clicked
  "Returns the current position of the mouse on a click.

  Returns a new position from the mouse within the canvas frame on mouse click,
  otherwise returns the previous location if outside of bounds or no buttons
  currently pressed."
  [last-position]
  (if (q/mouse-pressed?)
    (let [pos (mouse-position)]
      (if (g/contains-point? (screen-rect) pos)
        pos
        last-position))
    last-position))

(defn plot [shape points]
  (doseq [p points]
    (shape p)))

(defn draw-shape [vertices]
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape :close))

(defn draw-curve-shape [vertices]
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/curve-vertex x y))
  (q/end-shape :close))

(defn draw-polygon [poly]
  (draw-shape (g/vertices poly)))

(defn draw-path [vertices]
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape))

(defn draw-curve-path [points]
  (q/begin-shape)
  (doseq [[x y] points]
    (q/curve-vertex x y))
  (q/end-shape))

(defn color-if
  "Apply `set-color-fn` with `color` if color is a sequence."
  [set-color-fn color]
  (when (seq color) (apply set-color-fn color)))

(defn draw-triangle
  ([[ax ay] [bx by] [cx cy]]
   (q/triangle ax ay bx by cx cy))
  ([[[ax ay] [bx by] [cx cy]]]
   (q/triangle ax ay bx by cx cy)))

(defn lerp-line [[x y] [x' y'] amt]
  (q/line x y (q/lerp x x' amt) (q/lerp y y' amt)))

;; TODO move/combine with transition-interval logic
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

(defn circle
  ([{[x y] :p r :r}] (q/ellipse x y r r)) ;; gc/circle
  ([[x y] radius] (q/ellipse x y radius radius))
  ([x y radius] (q/ellipse x y radius radius)))

(defn rectangle
  "Adapter to convert g/rect to quil/rect"
  ([{[x y] :p [w h] :size}] (q/rect x y w h)))
