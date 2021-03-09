(ns shimmers.sketches.k-means
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]))

(defn rel-h [p]
  (* (q/height) p))

(defn rel-w [p]
  (* (q/width) p))

(defn magnitude [v]
  (Math/sqrt (reduce + (map * v v))))

;; https://en.wikipedia.org/wiki/Cosine_similarity
(defn cos-similarity [v1 v2]
  (/ (reduce + (map * v1 v2))
     (* (magnitude v1) (magnitude v2))))

(defn draw-shape [{:keys [position shape color]}]
  (apply q/fill color)
  (cq/draw-shape (geom/vertices (geom/translate shape position))))

(defn make-shape []
  {:position (gv/vec2 (rel-w (rand)) (rel-h (rand)))
   :shape (gt/triangle2 [0 0] [0 (q/random 13 21)] [(q/random 13 21) 0])
   :color [(mod (q/random 1080) 360) (q/random 0.5 0.8) (q/random 0.5 0.8) 0.1]})

(defn setup []
  (q/color-mode :hsl 360 1.0 1.0 1.0)
  {:shapes (repeatedly 64 make-shape)})

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (q/no-stroke)
  (doseq [shape shapes]
    (draw-shape shape)))

(defn ^:export run-sketch []
  (q/defsketch k-means
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
