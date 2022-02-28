(ns shimmers.sketches.chaikin-demo
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.chaikin :as chaikin]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.utils.subdiv :as gsd]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.triangle :as gt]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shapes (map (fn [v] (g/translate v (cq/rel-vec 0.05 0.05)))
                    [(rect/rect (cq/rel-pos 0.0 0.0) (cq/rel-pos 0.1 0.1))
                     (gt/triangle2 (cq/rel-pos 0.0 0.0) (cq/rel-pos 0.0 0.1) (cq/rel-pos 0.1 0.0))])]
    {:shapes shapes
     :shape (first shapes)}))

(defn update-state [{:keys [shapes] :as state}]
  (let [fc (/ (q/frame-count) (* 10 60))]
    (assoc state :shape (nth shapes (mod (int fc) (count shapes))))))

(defn text-at [s [x y]]
  (q/push-style)
  (q/fill 0)
  (q/text s x y)
  (q/pop-style))

(defn draw-at
  ([vertices pos] (draw-at nil vertices pos))
  ([desc vertices pos]
   (let [translated (map (fn [v] (g/translate v pos)) vertices)]
     (cq/draw-path translated)
     (cq/circle (first translated) 3.0)
     (when desc
       (text-at desc (g/translate pos (cq/rel-vec 0.05 0.2)))))))

(def gsd-chaikin (partial gsd/subdivide-closed (:chaikin gsd/schemes)))
(def gsd-bezier (partial gsd/subdivide-closed (:cubic-bezier gsd/schemes)))

(defn draw [{:keys [shape]}]
  (q/background 1.0)
  (let [vertices (g/vertices shape)]
    (draw-at "Original" vertices (gv/vec2))
    (draw-at "Chaikin Closed" (chaikin/chaikin-closed vertices 0.25) (cq/rel-vec 0.2 0.0))
    (draw-at "2 iters" (chaikin/chaikin 0.25 true 2 vertices) (cq/rel-vec 0.4 0.0))
    (draw-at "3 iters" (chaikin/chaikin 0.25 true 3 vertices) (cq/rel-vec 0.6 0.0))
    (draw-at "4 iters" (chaikin/chaikin 0.25 true 4 vertices) (cq/rel-vec 0.8 0.0))
    (draw-at "Chaikin Open" (chaikin/chaikin-open vertices 0.25) (cq/rel-vec 0.2 0.2))
    (draw-at (chaikin/chaikin 0.25 false 2 vertices) (cq/rel-vec 0.4 0.2))
    (draw-at (chaikin/chaikin 0.25 false 3 vertices) (cq/rel-vec 0.6 0.2))
    (draw-at (chaikin/chaikin 0.25 false 4 vertices) (cq/rel-vec 0.8 0.2))
    (draw-at "subdiv Chaikin" (gsd-chaikin vertices) (cq/rel-vec 0.2 0.4))
    (draw-at (gsd-chaikin (gsd-chaikin vertices)) (cq/rel-vec 0.4 0.4))
    (draw-at (gsd-chaikin (gsd-chaikin (gsd-chaikin vertices))) (cq/rel-vec 0.6 0.4))
    (draw-at (gsd-chaikin (gsd-chaikin (gsd-chaikin (gsd-chaikin vertices)))) (cq/rel-vec 0.8 0.4))
    (draw-at "subdiv Cubic-Bezier" (gsd-bezier vertices) (cq/rel-vec 0.2 0.6))
    (draw-at (gsd-bezier (gsd-bezier vertices)) (cq/rel-vec 0.4 0.6))
    (draw-at (gsd-bezier (gsd-bezier (gsd-bezier vertices))) (cq/rel-vec 0.6 0.6))
    (draw-at (gsd-bezier (gsd-bezier (gsd-bezier (gsd-bezier vertices)))) (cq/rel-vec 0.8 0.6))))

(sketch/defquil chaikin-demo
  :created-at "2021-08-31"
  :tags #{:demo}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
