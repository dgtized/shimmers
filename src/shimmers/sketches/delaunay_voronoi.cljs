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

(defn bisect-line
  [[p q]]
  (let [[mid-x mid-y] (tm/div (tm/+ p q) 2)
        reciprocal-slope (let [d (tm/- q p)]
                           (if (not= 0 (:x d))
                             (/ -1 (geom/slope-xy d))
                             0))
        b (- mid-y (* reciprocal-slope mid-x))]
    [reciprocal-slope b]))

;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
(defn intercept-point [[a c] [b d]]
  (when-not (zero? (- a b))
    (let [x (/ (- d c) (- a b))]
      [x (+ (* a x) b)])))

(defn voronoi-edges [neighborhood vertex]
  (for [triangle (get neighborhood vertex)]
    (let [[a b] (for [p triangle
                      :when (not= p vertex)]
                  (bisect-line [vertex p]))]
      (intercept-point a b))))

(comment
  (let [points (generate-points 5 rand)
        triangles (delaunay/triangulate points)
        neighborhood (neighboring-triangles triangles)
        vertex (first points)]
    [vertex (neighboring-vertices neighborhood vertex)])

  (let [points (generate-points 8 rand)
        triangles (delaunay/triangulate points)
        neighborhood (neighboring-triangles triangles)
        vertex (first points)]
    [vertex (voronoi-edges neighborhood vertex)]))

(defn bisect
  "Calculate intersection points for circles of radius (p - q) centered at p and q."
  [[p q]]
  (let [d (tm/- q p)]
    (mapv (fn [θ] (tm/+ p (geom/rotate d θ)))
          [(/ Math/PI 3) (* 5 (/ Math/PI 3))])))

(defn plot [[m b]]
  (q/push-style)
  (q/stroke-weight 0.1)
  (let [w (q/width)]
    (q/line 0 b w (+ (* w m) b)))
  (q/pop-style))

(defn setup []
  (q/no-loop)
  (let [points (generate-points 12 #(q/random 0.15 0.85))]
    {:points points
     :triangles (delaunay/triangulate points)}))

(defn update-state [state]
  state)

(defn draw-bisect [edge]
  (let [line (gl/line2 (bisect edge))
        [a b] (:points (geom/scale-size line 0.02))]
    (q/line a b)))

(defn draw [{:keys [points triangles]}]
  (println "draw")
  (q/background 255)

  (q/stroke-weight 0.5)
  (q/no-fill)
  (doseq [triangle triangles
          :let [view-pts (map cq/rel-pos triangle)]]
    (apply q/triangle (flatten view-pts))
    (doseq [edge (geom/edges (apply gt/triangle2 view-pts))]
      #_(plot (bisect-line edge))
      (draw-bisect edge))
    )

  (q/ellipse-mode :radius)
  (q/fill 0 0 0)
  (doseq [p points
          :let [[x y] (cq/rel-pos p)]]
    (q/ellipse x y 1 1))

  (q/stroke 255 0 0)
  (q/no-fill)
  (let [neighborhood (neighboring-triangles triangles)]
    (doseq [point [(rand-nth points)]]
      (let [edge-points (voronoi-edges neighborhood point)]
        (println point)
        (println edge-points)
        (let [[x y] (cq/rel-pos point)]
          (q/ellipse x y 2 2))

        (when (> (count edge-points) 2)
          (q/begin-shape)
          (doseq [vertex edge-points]
            (let [[x y] (cq/rel-pos vertex)]
              (q/ellipse x y 2 2))
            (apply q/vertex (cq/rel-pos vertex)))
          (q/end-shape)))))
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
