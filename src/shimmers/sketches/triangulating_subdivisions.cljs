(ns shimmers.sketches.triangulating-subdivisions
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as geom]))

(defrecord Triangle [a b c])
(defrecord Line [a b])

(defn triangle->lines [{:keys [a b c]}]
  [(->Line a b)
   (->Line b c)
   (->Line c a)])

(defn subdivide-line [{:keys [a b]}]
  (v/add b (v/scale (v/sub a b) (q/random 0.25 0.75))))

(defn subdivide-triangle [{:keys [a b c]}]
  (let [[a b c] (shuffle [a b c])
        m (subdivide-line (->Line a b))]
    [(->Triangle a m c)
     (->Triangle b m c)]))

(defn setup []
  (q/frame-rate 2)
  (let [top (v/vec2 (* 0.5 (q/width)) (* 0.1 (q/height)))
        left (v/vec2 (* 0.1 (q/width)) (* 0.9 (q/height)))
        right (v/vec2 (* 0.9 (q/width)) (* 0.9 (q/height)))]
    {:triangles [(->Triangle top left right)]}))

(defn update-state [{:keys [triangles] :as state}]
  (let [[s & r] (shuffle triangles)]
    (assoc state :triangles (into (subdivide-triangle s) r))))

(defn draw [{:keys [triangles]}]
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
