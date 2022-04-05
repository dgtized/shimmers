(ns shimmers.sketches.slashes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)
        hatch (partial clip/variable-hatching bounds)]
    {:slashes
     (concat (hatch (tm/random 5.0 6.0) 0
                    (int (tm/random 6 16)) (constantly 10) (constantly 1))
             (hatch (tm/random 5.0 6.0) (cq/rel-w 0.8)
                    10 #(tm/random 5 15) #(tm/random 0.8 6))
             (hatch (tm/random 3.5 4.5) (cq/rel-w 0.4)
                    (int (tm/random 4 16)) #(tm/random 5 15) #(tm/random 0.5 4))
             (hatch (tm/random 3.5 4.5) (* (g/width bounds) 1.3)
                    8 #(tm/random 5 15) #(tm/random 0.5 2)))}))

(defn update-state [state]
  state)

(defn draw [{:keys [slashes]}]
  (q/background 1.0)
  (doseq [{[p q] :points width :width} slashes]
    (q/stroke-weight width)
    (q/line p q)))

(sketch/defquil slashes
  :created-at "2021-08-20"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
