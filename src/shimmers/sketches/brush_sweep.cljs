(ns shimmers.sketches.brush-sweep
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t (fn [t] (mod (+ t (tm/random 0.05)) 100.0))))

(defn hairs [line]
  (let [hair (-> (gt/triangle2 [0 0] [3 7] [7 5])
                 g/center
                 (g/scale-size 5))]
    (->> (if true
           (g/sample-uniform line 20 true)
           (repeatedly 64 #(g/random-point line)))
         (map (fn [p] (-> hair
                         (g/rotate (* tm/TWO_PI (tm/random)))
                         (g/translate p)))))))

(defn draw [{:keys [t]}]
  (q/stroke-weight 0.5)
  (q/stroke 0 0.01)
  (let [t' (/ t 100)]
    (q/fill (tm/mix* 0.5 0.9 (+ t' (tm/random -0.05 0.05)))

            (tm/mix-circular 0.5 0.8 t')
            (tm/random 0.5 0.7)
            0.02)
    (let [pos (cq/rel-vec (+ -0.1
                             (/ (tm/smoothstep* 0.3 0.4 t') 30)
                             (/ (- (tm/step* 0.6 t')) 30))
                          (* 0.01 t))
          slope (* (q/random 0.1) t')
          line (gl/line2 pos
                         (gv/vec2 (q/width) (+ (* slope (q/width)) (:y pos))))]
      (doseq [hair (shuffle (hairs line))]
        (cq/draw-polygon hair)))))

(sketch/defquil brush-sweep
  :created-at "2021-04-12"
  :size [1200 900]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
