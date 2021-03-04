(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as poly]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]))

(defn setup []
  (q/frame-rate 1)
  (let [w (q/width)
        h (q/height)]
    {:shape (rect/rect (* 0.2 w) (* 0.2 h) (* 0.5 w) (* 0.6 h))}))

(defn update-state [state]
  state)

(defn draw-polygon [poly]
  (q/begin-shape)
  (doseq [p (geom/vertices poly)]
    (apply q/vertex p))
  (q/end-shape :close))

(defn rotate-around-centroid [polygon t]
  (-> polygon
      (geom/center (gv/vec2 0 0))
      (geom/rotate t)
      (geom/translate (geom/centroid polygon))))

(defn generate-strokes [brush random-position n]
  (repeatedly n #(rotate-around-centroid
                  (geom/translate brush (random-position))
                  (q/random 0 Math/PI))))

(defn draw [{:keys [shape]}]
  (q/background 255)
  ;; (q/no-loop)
  ;; (q/background 255 0.1)
  (let [w (q/width)
        h (q/height)
        poly (geom/as-polygon shape)
        brush (geom/as-polygon (gt/triangle2 (gv/vec2 0 0) (gv/vec2 (* 0.05 w) 0) (gv/vec2 0 (* 0.05 h))))]
    ;; (draw-polygon poly)
    (q/stroke-weight 0.1)
    (q/fill 0 0.5 0.8 0.1)
    (doseq [copy (concat (generate-strokes brush #(geom/random-point-inside shape) 10000)
                         (generate-strokes brush #(geom/random-point shape) 2000))]
      (draw-polygon copy))))

(defn ^:export run-sketch []
  (q/defsketch noisy-shapes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
