(ns shimmers.sketches.woven
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   [shimmers.math.geometry.triangle :as triangle]))

(defn gen-threads [n pass]
  (for [t (range n)]
    (let [o (/ (float (inc t)) (inc n))]
      (if (even? pass)
        [(cq/rel-vec -0.1 o) v/right]
        [(cq/rel-vec o -0.1) v/up]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [pass 0
        n 12]
    {:n n
     :pass pass
     :triangles (gen-threads n pass)
     :screen (cq/screen-rect)
     :t (q/millis)}))

(defn outside? [screen [pos dir]]
  (and (not (g/contains-point? screen pos))
       (case dir
         v/right (> (:x pos) (cq/rel-w 1.1))
         v/up (> (:y pos) (cq/rel-h 1.1)))))

(defn update-pos [dt [pos dir]]
  [(tm/+ pos (tm/* dir (* 0.1 dt)))
   dir])

(defn update-state [{:keys [triangles screen pass n t] :as state}]
  (let [dt (- (q/millis) t)]
    (-> (if (every? (partial outside? screen) triangles)
          (-> state
              (update :pass inc)
              (assoc :triangles (gen-threads n (inc pass))))
          state)
        (update :t + dt)
        (update :triangles (partial map (partial update-pos dt))))))

(defn draw [{:keys [triangles n t]}]
  (q/fill 0.0 0.1)
  (q/stroke 0.0 0.1)
  (println (first triangles))
  (doseq [[pos _] triangles]
    (let [triangle (triangle/inscribed-equilateral pos (cq/rel-h (/ 1.0 (* 2 n))) 0.0)]
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
