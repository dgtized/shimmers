(ns shimmers.sketches.delaunay-voronoi
  (:require [clojure.set :as set]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.utils.delaunay :as delaunay]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn generate-points [n dist]
  (repeatedly n #(gv/vec2 (dist) (dist))))

(defn neighboring-triangles
  "Map every distinct vertex in triangles to the set of neighboring triangles that
  include that point as a vertex.

  (neighborhood [[a b c] [a b d]])
  =>
  {a #{[a b c] [a b d]}
   b #{[a b c] [a b d]}
   c #{[a b c]}
   d #{[a b d]}}"
  [triangles]
  (->> (for [triangle triangles
             vertex triangle]
         [vertex #{triangle}])
       (reduce (fn [m [vertex triangle]]
                 (update m vertex set/union triangle))
               {})))

(defn neighboring-vertices [neighborhood vertex]
  (->> vertex
       (get neighborhood)
       (map set)
       (reduce set/union)
       (remove #{vertex})))

(comment
  (let [a (rand)
        b (rand)]
    (= (gv/vec2 a b) (gv/vec2 a b)))

  ;; (set v) is different than #{v}
  (let [v (gv/vec2 0 2)]
    [#{v} (set v)])

  (let [points (generate-points 5 rand)
        triangles (delaunay/triangulate points)
        neighborhood (neighboring-triangles triangles)
        vertex (first points)]
    [vertex (neighboring-vertices neighborhood vertex)]))

(defn bisect
  "Calculate intersection points for circles of radius (p - q) centered at p and q."
  [[p q]]
  (let [d (tm/- q p)]
    (mapv (fn [θ] (tm/+ p (geom/rotate d θ)))
          [(/ Math/PI 3) (* 5 (/ Math/PI 3))])))

(defn bisect-line
  [[p q]]
  (let [[mid-x mid-y] (tm/div (tm/+ p q) 2)
        reciprocal-slope (let [d (tm/- q p)]
                           (if (not= 0 (:x d))
                             (/ -1 (geom/slope-xy d))
                             0))
        b (- mid-y (* reciprocal-slope mid-x))]
    [reciprocal-slope b]))

(defn plot [[m b]]
  (q/push-style)
  (q/stroke-weight 0.1)
  (let [w (q/width)]
    (q/line 0 b w (+ (* w m) b)))
  (q/pop-style))

(defn setup []
  (q/no-loop)
  (let [points (generate-points 6 #(q/random 0.15 0.85))]
    {:points points
     :triangles (delaunay/triangulate points)}))

(defn update-state [state]
  state)

(defn draw [{:keys [points triangles]}]
  (q/background 255)

  (q/stroke-weight 0.5)
  (q/no-fill)
  (doseq [triangle triangles
          :let [view-pts (map cq/rel-pos triangle)]]
    (apply q/triangle (flatten view-pts))
    (doseq [edge (geom/edges (apply gt/triangle2 view-pts))
            :let [line (gl/line2 (bisect edge))
                  [a b] (:points (geom/scale-size line 0.02))]]
      (plot (bisect-line edge))
      (q/line a b)))

  (q/ellipse-mode :radius)
  (q/fill 0 0 0)
  (doseq [p points
          :let [[x y] (cq/rel-pos p)]]
    (q/ellipse x y 1 1))

  (let [neighborhood (neighboring-triangles triangles)]
    (println (first points))
    (println (neighboring-vertices neighborhood (last points))))
  )

(defn ^:export run-sketch []
  ;; 20210321
  (q/defsketch delaunay-voronoi
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
