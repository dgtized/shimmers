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
                               {:rows 10 :cols 8})
   :lines []})

(defn update-state [{:keys [rectangles lines] :as state}]
  (if (empty? rectangles)
    state
    (let [rect (rand-nth rectangles)
          hatches (clip/hatch-rectangle rect (tm/random 5.0 10.0)
                                        (tm/random 0 tm/TWO_PI))]
      (assoc state
             :rectangles (remove #{rect} rectangles)
             :lines (into lines hatches)))))

(defn draw [{:keys [rectangles lines]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [{[x y] :p [w h] :size} rectangles]
    (q/rect x y w h))
  (doseq [{[p q] :points} lines]
    (q/line p q)))

(sketch/defquil hatched-rectangles
  :created-at "2021-08-17"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
