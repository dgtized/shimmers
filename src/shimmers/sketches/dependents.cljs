(ns shimmers.sketches.dependents
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
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
               :connect-surfaces true
               :extrude-shapes false
               :show-debug false}))

(defn update-polygons [particles]
  (for [{:keys [pos heading shape] :as particle} particles]
    (assoc particle :poly (g/center (g/rotate shape heading) pos))))

(defn gen-particles [n bounds]
  (for [pos (rp/random-points bounds n)]
    {:pos pos
     :vel (v/polar (tm/clamp (dr/gaussian 0.2 0.05) 0.05 0.5) (dr/random-tau))
     :heading (dr/random-tau)
     :spin (dr/gaussian 0.0 0.066)
     :shape (dr/rand-nth [
                          (triangle/inscribed-equilateral {:r 10} 0)
                          (rect/rect 0 0 5 15)])}))

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
     :particles (update-polygons
                 (gen-particles 8 (rect/rect (cq/rel-vec 0.1 0.33)
                                             (cq/rel-vec 0.9 0.66))))
     :t0 (q/millis)
     :t (q/millis)
     :max-distance (g/dist (cq/rel-vec 0 0) (cq/rel-vec 1 1))}))

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
              (tm/+ forces (tm/div force (max 0.1 (tm/mag-squared force))))))
          (gv/vec2)
          repulsors))

(defn update-particles [particles repulsors bounds _t dt]
  (for [{:keys [pos vel heading spin] :as particle} particles]
    (let [pos' (tm/+ pos (tm/* vel dt))]
      (if (g/contains-point? bounds pos')
        (assoc particle
               :pos pos'
               :vel (tm/* (tm/+ vel (tm/* (forces pos repulsors) (* 0.01 dt)))
                          0.999)
               :last-pos pos
               :heading (+ heading (* 0.01 spin dt)))
        (let [bounce (reflect bounds particle)]
          (assoc particle
                 :pos (tm/+ pos (tm/* bounce dt))
                 :last-pos pos
                 :vel bounce
                 :heading (+ heading (* 0.01 spin dt))
                 :spin (* -1 spin)))))))

(defn update-state [{:keys [t bounds repulsors] :as state}]
  (let [dt (- (q/millis) t)]
    (-> state
        (update :particles update-particles repulsors bounds t dt)
        (update :particles update-polygons)
        (update :t + dt))))

(defonce defo (debug/state {}))

(defn draw [{:keys [particles t t0 max-distance]}]
  (when (> (- t t0) 16000)
    (q/no-loop))
  (q/no-fill)
  (swap! defo assoc :particles particles)
  (let [{:keys [connect-surfaces extrude-shapes]} @ui-state]
    (doseq [{:keys [pos last-pos poly] :as _particle} particles]
      (when last-pos
        (q/stroke 0 0.75 0.33 0.15)
        (q/line last-pos pos))

      (when extrude-shapes
        (cq/draw-polygon poly))

      (let [neighbors (take 3 (drop 1 (sort-by (fn [part] (g/dist-squared (:pos part) pos)) particles)))]
        (doseq [[i neighbor] (map-indexed vector neighbors)
                :let [distance (g/dist pos (:pos neighbor))
                      pct-dist (/ distance max-distance)]]
          (when (< pct-dist (* 0.5 (/ (float (inc i)) 4)))
            (q/stroke 0 (* (- 1.0 pct-dist) (/ 1.0 (math/pow 4 (inc i)))))
            (if connect-surfaces
              (q/line (g/closest-point (:poly neighbor) pos)
                      (g/closest-point poly (:pos neighbor)))
              (q/line pos (:pos neighbor)))))))))

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
      [ctrl/checkbox-after ui-state "Extrude Shapes" [:extrude-shapes]]
      [ctrl/checkbox-after ui-state "Connect Surfaces" [:connect-surfaces]]
      [ctrl/checkbox-after ui-state "Debug" [:show-debug]]]]
    (when (:show-debug @ui-state)
      [debug/display defo])]])

(sketch/definition dependents
    {:created-at "2023-11-02"
     :tags #{}
     :type :quil}
  (ctrl/mount page))
