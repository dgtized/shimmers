(ns shimmers.sketches.dependents
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:screen-size "900x600"
               :show-debug false}))

(defn gen-particles [n bounds]
  (for [pos (rp/random-points bounds n)]
    {:pos pos
     :vel (v/polar (dr/random 0.05 0.25) (dr/random-tau))}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:bounds bounds
     :repulsors [(gc/circle (cq/rel-vec 0.15 0.15) (cq/rel-h 0.05))
                 (gc/circle (cq/rel-vec 0.5 0.15) (cq/rel-h 0.05))
                 (gc/circle (cq/rel-vec 0.85 0.15) (cq/rel-h 0.05))
                 (gc/circle (cq/rel-vec 0.15 0.85) (cq/rel-h 0.05))
                 (gc/circle (cq/rel-vec 0.5 0.85) (cq/rel-h 0.05))
                 (gc/circle (cq/rel-vec 0.85 0.85) (cq/rel-h 0.05))]
     :particles (gen-particles 8 (rect/rect (cq/rel-vec 0.1 0.33)
                                            (cq/rel-vec 0.9 0.66)))
     :seconds (dr/chance 0.4)
     :t0 (q/millis)
     :t (q/millis)}))

(defn reflect [bounds {:keys [pos vel]}]
  (let [[x y] pos
        [dx dy] vel]
    (cond (< x (rect/left bounds))
          (gv/vec2 (- dx) dy)
          (>= x (rect/right bounds))
          (gv/vec2 (- dx) dy)
          (< y (rect/bottom bounds))
          (gv/vec2 dx (- dy))
          (>= y (rect/top bounds))
          (gv/vec2 dx (- dy))
          :else
          vel)))

(defn forces [pos repulsors]
  (reduce (fn [forces rep]
            (let [force (tm/- pos (:p rep))]
              (tm/+ forces (tm/div force (max 1 (tm/mag-squared force))))))
          (gv/vec2)
          repulsors))

(defn update-particles [particles repulsors bounds _t dt]
  (for [{:keys [pos vel] :as particle} particles]
    (let [pos' (tm/+ pos (tm/* vel dt))]
      (if (g/contains-point? bounds pos')
        (assoc particle
               :pos pos'
               :vel (tm/* (tm/+ vel (tm/* (forces pos repulsors) (* 0.0075 dt)))
                          0.999)
               :last-pos pos)
        (let [bounce (reflect bounds particle)]
          (assoc particle
                 :pos (tm/+ pos (tm/* bounce dt))
                 :last-pos pos
                 :vel bounce))))))

(defn update-state [{:keys [t bounds repulsors] :as state}]
  (let [dt (- (q/millis) t)]
    (-> state
        (update :particles update-particles repulsors bounds t dt)
        (update :t + dt))))

(defonce defo (debug/state {}))

(defn draw [{:keys [particles seconds t t0]}]
  (when (> (- t t0) 16000)
    (q/no-loop))
  (swap! defo assoc :particles particles)
  (doseq [{:keys [pos last-pos] :as _particle} particles]
    (when last-pos
      (q/stroke 0 0.75 0.33 0.33)
      (q/line last-pos pos))

    (let [neighbors (drop 1 (sort-by (fn [part] (g/dist-squared (:pos part) pos)) particles))]
      (let [closest (first neighbors)]
        (when (< (g/dist pos (:pos closest)) (/ (q/height) 4))
          (q/stroke 0 0.2)
          (q/line pos (:pos closest))))
      (let [farther (second neighbors)]
        (when (and seconds (< (g/dist pos (:pos farther)) (/ (q/height) 2)))
          (q/stroke 0 0.02)
          (q/line pos (:pos farther)))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.flexcols
    [:div
     [:p.readable-width
      "Bounce a few particles through the space, drawing lines between closest
     pairs if they are less than a threshold. The striation arises from the
     variations in the sample rate between frames to draw a new line. Stop after
     16s."]
     [ctrl/container
      [ctrl/dropdown ui-state "Screen Size" [:screen-size]
       (screen/sizes)
       {:on-change #(view-sketch/restart-sketch :dependents)}]
      [ctrl/checkbox-after ui-state "Debug" [:show-debug]]]]
    (when (:show-debug @ui-state)
      [debug/display defo])]])

(sketch/definition dependents
    {:created-at "2023-11-02"
     :tags #{}
     :type :quil}
  (ctrl/mount page))
