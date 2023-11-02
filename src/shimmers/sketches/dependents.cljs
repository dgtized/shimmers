(ns shimmers.sketches.dependents
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:screen-size "900x600"}))

(defn gen-particles [n bounds]
  (for [pos (rp/random-points bounds n)]
    {:pos pos :vel (v/polar (dr/random 0.05 0.2) (dr/random-tau))}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :particles (gen-particles 8 bounds)
     :t0 (q/millis)
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

(defn draw [{:keys [particles t t0]}]
  (when (> (- t t0) 16000)
    (q/no-loop))
  (doseq [{:keys [pos last-pos] :as _particle} particles]
    (when last-pos
      (q/stroke 0 0.75 0.33 0.33)
      (q/line last-pos pos))

    (let [closest (first (drop 1 (sort-by (fn [part] (g/dist-squared (:pos part) pos)) particles)))]
      (when (< (g/dist pos (:pos closest)) (/ (q/height) 4))
        (q/stroke 0 0.2)
        (q/line pos (:pos closest))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [ctrl/container
    [ctrl/dropdown ui-state "Screen Size" [:screen-size]
     (screen/sizes)
     {:on-change #(view-sketch/restart-sketch :dependents)}]]])

(sketch/definition dependents
    {:created-at "2023-11-02"
     :tags #{}
     :type :quil}
  (ctrl/mount page))
