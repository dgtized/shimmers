(ns shimmers.sketches.lifecycle-of-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.utils :as gu]
   [thi.ng.math.core :as tm]))

(defn correspondences
  "Given `triangles`, a vector of vectors of triangles, and `n`, the maximum
  number of triangles to correspond, generate a vector of index vectors, where
  each indice maps back into the corresponding triangle vector."
  [triangles n]
  (->> triangles
       (map (comp dr/shuffle (partial take n) cycle range count))
       (apply mapv vector)))

(defn random-tessellation [u1 u2]
  (let [max-triangles (* 2 u1 u2)]
    (fn [shape]
      (if (dr/chance 0.33)
        (g/tessellate (g/bounding-circle shape)
                      (dr/random-int (* 0.3 max-triangles) (* 0.8 max-triangles)))
        (g/tessellate shape {:cols (dr/random-int 3 u1)
                             :rows (dr/random-int 2 u2)})))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [[u1 u2] [6 5]
        max-triangles-per-shape (* 2 u1 u2)
        screen (cq/screen-rect 0.95)
        initial-shapes (-> screen
                           (g/subdivide {:rows 5 :cols 4})
                           dr/shuffle)
        core-shapes (->> initial-shapes
                         (drop (* 0.2 (count initial-shapes)))
                         (mapv #(g/scale-size % (dr/random 0.25 1.5)))
                         (gu/fit-all-into-bounds screen)
                         dr/shuffle)
        shapes (mapv (fn [s]
                       (let [dt (dr/weighted {(constantly 0.0) 2
                                              #(dr/random -5 5) 2
                                              #(dr/random -10 10) 2
                                              #(dr/random -25 -50) 1
                                              #(dr/random 25 50) 1})
                             theta (dr/weighted {(constantly 0.0) 3.0
                                                 #(dr/random 0 tm/TWO_PI) 1.0})]
                         (assoc s
                                :theta (theta)
                                :dtheta (dt)))) core-shapes)
        triangles (map (random-tessellation u1 u2) shapes)]
    {:t 0.0
     :shapes shapes
     :triangles triangles
     :correspondences (correspondences triangles max-triangles-per-shape)}))

(defn rotate-all [triangles shapes t]
  (letfn [(rotate [shape-tris {:keys [theta dtheta] :as shape}]
            (let [shape-center (g/centroid shape)]
              (mapv (fn [tri]
                      (geometry/rotate-around tri shape-center (+ theta (* t dtheta))))
                    shape-tris)))]
    (mapv rotate triangles shapes)))

(defn update-state [{:keys [t triangles shapes] :as state}]
  (-> state
      (update :t + (* 0.0003 (+ 0.25 (q/noise t))))
      (assoc :rotated-triangles (rotate-all triangles shapes t))))

(defn draw [{:keys [t rotated-triangles correspondences]}]
  (q/stroke-weight 0.8)
  (let [n-states (count rotated-triangles)
        raw-offset (* (mod t 1.0) n-states)
        o1 (int raw-offset)
        t-delta (- raw-offset o1)]
    (when (= 0 (mod (q/frame-count) 5))
      (doseq [correlate correspondences
              :let [triset (map (fn [i tessellation] (nth tessellation i)) correlate rotated-triangles)
                    [pts1 pts2] (map (comp :points (partial nth triset))
                                     [o1 (mod (inc o1) n-states)])
                    vertices (map (fn [v1 v2] (tm/mix v1 v2 t-delta)) pts1 pts2)]]
        (apply cq/draw-triangle vertices)))))

(sketch/defquil lifecycle-of-shapes
  :created-at "2021-12-28"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
