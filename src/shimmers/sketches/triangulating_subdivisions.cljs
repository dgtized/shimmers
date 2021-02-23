(ns shimmers.sketches.triangulating-subdivisions
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.core :as geom]))

(defrecord Line [a b])

(defn triangle->lines [t]
  (map (fn [[a b]] (->Line a b)) (geom/edges t)))

(defn subdivide-line [{:keys [a b]}]
  (v/add b (v/scale (v/sub a b) (q/random 0.25 0.75))))

(defn subdivide-triangle [t]
  (let [[a b c] (shuffle (:points t))
        m (subdivide-line (->Line a b))]
    [(gt/triangle2 a m c)
     (gt/triangle2 b m c)]))

(defn initial-conditions []
  (let [top (v/vec2 (* 0.5 (q/width)) (* 0.1 (q/height)))
        left (v/vec2 (* 0.1 (q/width)) (* 0.9 (q/height)))
        right (v/vec2 (* 0.9 (q/width)) (* 0.9 (q/height)))]
    {:triangles [(gt/triangle2 top left right)]}))

(defn setup []
  (q/frame-rate 10)
  (initial-conditions))

(defn update-state [{:keys [triangles] :as state}]
  (if (> (count triangles) 100)
    (initial-conditions)
    (let [[s & r] (shuffle triangles)]
      (assoc state :triangles (into (subdivide-triangle s) r)))))

(defn draw [{:keys [triangles]}]
  (q/background 255)
  (doseq [line (mapcat triangle->lines triangles)]
    (q/line (:a line) (:b line))))

(defn ^:export run-sketch []
  (q/defsketch triangulating-subdivisions
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
