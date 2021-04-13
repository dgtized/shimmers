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
  {:t 0})

(defn update-state [state]
  (update state :t (fn [t] (mod (+ t (tm/random 0.05)) 100.0))))

(defn hairs [line]
  (let [hair (-> (gt/triangle2 [0 0] [3 7] [7 5])
                 geom/center
                 (geom/scale-size 5))]
    (->> (geom/sample-uniform line 15 true)
         (map (fn [p] (-> hair
                         (geom/rotate (* tm/TWO_PI (tm/random)))
                         (geom/translate p)))))))

(defn draw [{:keys [t]}]
  (q/stroke-weight 0.5)
  (q/stroke 0 0.01)
  (let [t' (/ t 100)]
    (q/fill (tm/mix* 0.5 0.9 (+ t' (tm/random -0.05 0.05)))

            (tm/mix-circular 0.5 0.8 t')
            (tm/random 0.5 0.7)
            0.02)
    (let [pos (gv/vec2 (cq/rel-pos (+ -0.1
                                      (/ (tm/smoothstep* 0.3 0.4 t') 30)
                                      (/ (- (tm/step* 0.6 t')) 40))
                                   (* 0.01 t)))
          slope (* 0.1 t')
          line (gl/line2 pos
                         (gv/vec2 (q/width) (+ (* slope (q/width)) (:y pos))))]
      (doseq [hair (shuffle (hairs line))]
        (cq/draw-shape (geom/vertices hair))))))

(defn ^:export run-sketch []
  ;; 20210412
  (q/defsketch brush-sweep
    :host "quil-host"
    :size [1200 900]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
