(ns shimmers.sketches.superposition
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]))

(defn rel-h [p]
  (* (q/height) p))

(defn rel-w [p]
  (* (q/width) p))

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn random-shape-at [position]
  (-> (gt/triangle2 [0 0] [0 13] [17 0])
      (geom/rotate (* 2 Math/PI (rand)))
      (geom/translate position)))

(defn setup []
  (let [shapes [(rect/rect (rel-w 0.15) (rel-h 0.15) (rel-w 0.3) (rel-h 0.3))
                (rect/rect (rel-w 0.55) (rel-h 0.55) (rel-w 0.3) (rel-h 0.3))]]
    {:shapes shapes
     :brushes (->> (fn [] (geom/random-point-inside (first shapes)))
                   (repeatedly 100)
                   (map random-shape-at))}))

(defn update-state [state]
  state)

(defn draw [{:keys [shapes brushes]}]
  (doseq [shape shapes]
    (draw-polygon shape))
  (doseq [brush brushes]
    (draw-polygon brush)))

(defn ^:export run-sketch []
  ;; 20210308
  (q/defsketch superposition
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
