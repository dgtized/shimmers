(ns shimmers.sketches.woven
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn gen-threads [n pass]
  (for [t (range n)]
    (let [o (+ (/ (float (inc t)) (inc n)) (dr/gaussian 0.0 (/ 0.4 (inc n))))]
      (case (mod pass 4)
        0 [(cq/rel-vec -0.1 o) 0.0 v/right]
        1 [(cq/rel-vec o -0.1) 0.0 v/up]
        2 [(cq/rel-vec 1.1 o) 0.0 v/left]
        3 [(cq/rel-vec o 1.1) 0.0 v/down]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [pass 0
        n 13]
    {:seed (cq/rel-vec (dr/random-vertex))
     :n n
     :pass pass
     :triangles (gen-threads n pass)
     :screen (cq/screen-rect)
     :t (q/millis)}))

(defn outside? [screen [pos _ dir]]
  (and (not (g/contains-point? screen pos))
       (case dir
         v/right (> (:x pos) (cq/rel-w 1.1))
         v/left (< (:x pos) (cq/rel-w -0.1))
         v/up (> (:y pos) (cq/rel-h 1.1))
         v/down (< (:y pos) (cq/rel-h -0.1)))))

(defn update-pos [t dt [pos rot dir]]
  [(tm/+ pos (tm/* dir (* 0.1 dt)))
   (+ rot (* (* 0.005 (Math/sin (* 0.0015 t))) dt))
   dir])

(defn update-state [{:keys [triangles screen pass t] :as state}]
  (let [dt (- (q/millis) t)]
    (-> (if (every? (partial outside? screen) triangles)
          (let [n (dr/rand-nth [11 12 13 17])]
            (-> state
                (update :pass inc)
                (assoc :n n
                       :triangles (gen-threads n (inc pass)))))
          state)
        (update :t + dt)
        (update :seed tm/+ (tm/* (gv/vec2 0.00001 0.00001) t))
        (update :triangles (partial map (partial update-pos t dt))))))

(defn draw [{:keys [seed triangles n]}]
  (doseq [[pos rot _] triangles]
    (let [n (apply q/noise (tm/* (tm/+ seed pos) 0.005))]
      (q/fill 0.0 (+ 0.0005 (* n 0.005)))
      (q/stroke 0.0 (+ 0.005 (* n 0.08))))
    (let [triangle (triangle/inscribed-equilateral pos (cq/rel-h (/ 0.33 (inc n))) rot)]
      (cq/draw-triangle (g/vertices triangle)))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition woven
  {:created-at "2023-10-25"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
