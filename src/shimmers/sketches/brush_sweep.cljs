(ns shimmers.sketches.brush-sweep
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.line :as gl]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:line (gl/line2 (cq/rel-pos -0.1 0) (cq/rel-pos 1.1 0))
   :t 0})

(defn update-state [state]
  (update state :t (fn [t] (mod (+ t (tm/random 0.2)) 100.0))))

(defn hairs [line]
  (let [hair (-> (gt/triangle2 [0 0] [3 7] [7 5])
                 geom/center
                 (geom/scale-size 2))]
    (->> (geom/sample-uniform line 25 true)
         (map (fn [p] (-> hair
                         (geom/rotate (* tm/TWO_PI (tm/random)))
                         (geom/translate p)))))))

(defn draw [{:keys [t line]}]
  (q/stroke-weight 0.5)
  (q/stroke 0 0.05)
  (q/no-fill)
  (let [offset (gv/vec2 (cq/rel-pos -0.1 (* 0.01 t)))]
    (doseq [hair (shuffle (hairs (geom/translate line offset)))]
      (cq/draw-shape (geom/vertices hair)))))

(defn ^:export run-sketch []
  ;; 20210412
  (q/defsketch brush-sweep
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
