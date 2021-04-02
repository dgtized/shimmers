(ns shimmers.sketches.delaunay-voronoi
  (:require [clojure.set :as set]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.utils.delaunay :as delaunay]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

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
      [x (+ (* a x) c)])))

(defn voronoi-edges [neighborhood vertex]
  (for [triangle (get neighborhood vertex)]
    (let [[a b] (for [p triangle
                      :when (not= p vertex)]
                  (bisect-line [vertex p]))]
      (intercept-point a b))))

(comment
  (let [points (geometry/generate-points 5 rand)
        triangles (delaunay/triangulate points)
        neighborhood (neighboring-triangles triangles)
        vertex (first points)]
    [vertex (neighboring-vertices neighborhood vertex)])

  (let [points (geometry/generate-points 8 rand)
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
  (let [points (geometry/generate-points 8 #(q/random 0.15 0.85))]
    {:points points
     ;; FIXME sometimes triangulation is not convex, suggesting not actually on hull?
     ;; maybe a problem with the triangulation function here?
     ;; See https://towardsdatascience.com/delaunay-triangulation-228a86d1ddad
     :triangles (delaunay/triangulate points)
     :hull (set (gp/convex-hull* points))}))

(defn update-state [state]
  state)

(defn draw-bisect [edge]
  (let [line (gl/line2 (bisect edge))
        [a b] (:points (geom/scale-size line 0.02))]
    (q/line a b)))

(defn relative-heading [centroid point]
  (geom/heading (tm/- (gv/vec2 point) centroid)))

(defn draw [{:keys [points triangles hull]}]
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

  (q/no-fill)
  (let [neighborhood (neighboring-triangles triangles)]
    (doseq [point (take 1 (shuffle points))
            :let [inside (if (get hull point) false true)]]
      (q/stroke 255 0 0)
      (let [[x y] (cq/rel-pos point)]
        (println [x y :inside inside])
        (q/ellipse x y 2 2))

      (let [neighbors (map cq/rel-pos (neighboring-vertices neighborhood point))
            centroid (gv/vec2 (cq/rel-pos point))
            edges (->> neighbors
                       (map gv/vec2)
                       (geometry/radial-sort centroid)
                       (map (fn [p] [centroid p])))
            bisects (map bisect-line edges)
            intersections (map intercept-point
                               bisects
                               (rest (if inside (cycle bisects) bisects)))]
        (doseq [edge edges
                :let [[x y] (second edge)]]
          (println {:edge [x y] :heading (geom/heading (tm/- (gv/vec2 x y) centroid))})
          (q/stroke 0 255 0)
          (q/ellipse x y 2 2)
          (q/stroke 0 0 0)
          (plot (bisect-line edge)))

        (q/stroke 0 0 255)
        (doseq [[x y] intersections]
          (println [x y :heading (geom/heading (tm/- (gv/vec2 x y) centroid))])
          (q/ellipse x y 2 2))

        (q/no-fill)
        (q/begin-shape)
        (doseq [p (sort-by (partial relative-heading centroid) intersections)]
          (apply q/vertex p))
        (if inside
          (q/end-shape :close)
          (q/end-shape)))
      ))
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
