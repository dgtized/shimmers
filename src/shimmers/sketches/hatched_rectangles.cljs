(ns shimmers.sketches.hatched-rectangles
  "Reconstruction of https://sighack.com/post/cohen-sutherland-line-clipping-algorithm examples"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.algorithm.line-clipping :as clip]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

;; rows/cols is sensitive and causes a freeze, not clear if in hatch-rectangle or clip-lines
(defn setup []
  (q/color-mode :hsl 1.0)
  {:rectangles (geom/subdivide (rect/rect (cq/rel-pos 0 0) (cq/rel-pos 1.0 1.0))
                               {:num 16})
   :lines []})

(defn update-state [{:keys [rectangles lines] :as state}]
  (if (empty? rectangles)
    state
    (let [rect (rand-nth rectangles)
          spacing (* (+ 0.6 (- 1.0 (/ (rect/top rect) (q/height))))
                     (tm/random 3.0 9.0))
          theta (tm/random 0 tm/TWO_PI)
          hatches (clip/hatch-rectangle rect spacing theta)]
      (assoc state
             :rectangles (remove #{rect} rectangles)
             :lines (into lines hatches)
             :draw hatches))))

(defn draw [{:keys [draw]}]
  ;; (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (doseq [{[p q] :points} draw]
    (q/line p q)))

(sketch/defquil hatched-rectangles
  :created-at "2021-08-17"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
