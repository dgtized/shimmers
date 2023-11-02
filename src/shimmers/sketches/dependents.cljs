(ns shimmers.sketches.dependents
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn gen-particles [n bounds]
  (for [pos (rp/random-points bounds n)]
    {:pos pos :vel (v/polar 0.5 (dr/random-tau))}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :particles (gen-particles 10 bounds)
     :t (q/millis)}))

(defn reflect [bounds {:keys [pos vel]}]
  (let [[x y] pos
        [dx dy] vel]
    (cond (>= x (rect/right bounds))
          (gv/vec2 (- dx) dy)
          (<= x (rect/left bounds))
          (gv/vec2 (- dx) dy)
          (<= y (rect/top bounds))
          (gv/vec2 dx (- dy))
          (>= y (rect/bottom bounds))
          (gv/vec2 dx (- dy)))))

(defn update-particles [particles bounds _t dt]
  (for [{:keys [pos vel] :as particle} particles]
    (let [pos' (tm/+ pos (tm/* vel dt))]
      (if (g/contains-point? bounds pos')
        (assoc particle
               :pos pos'
               :last-pos pos)
        (let [bounce (reflect bounds particle)]
          (assoc particle
                 :pos (tm/+ pos (tm/* bounce dt))
                 :last-pos pos
                 :vel bounce))))))

(defn update-state [{:keys [t bounds] :as state}]
  (let [dt (- (q/millis) t)]
    (-> state
        (update :particles update-particles bounds t dt)
        (update :t + dt))))

(defn draw [{:keys [particles]}]
  (doseq [{:keys [pos last-pos]} particles]
    (when last-pos
      (q/line last-pos pos))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition dependents
    {:created-at "2023-11-02"
     :tags #{}
     :type :quil}
  (ctrl/mount page))
