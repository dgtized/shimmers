(ns shimmers.sketches.unit-circle
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.string :as scs]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [radius (cq/rel-h 0.40)]
    {:radius radius
     :mouse (gv/vec2)
     :chain (chain/make-chain (gv/vec2) 8 (/ radius 8))}))

(defn update-state [state]
  (let [mouse (tm/- (cq/mouse-position) (cq/rel-vec 0.5 0.5))]
    (-> state
        (assoc :mouse mouse)
        (update :chain chain/chain-update (gv/vec2) mouse))))

(defn draw-unit [radius]
  (q/stroke-weight 1.0)
  (q/no-fill)
  (cq/circle [0 0] radius))

(defn draw-bisector [q weight]
  (q/stroke-weight weight)
  (q/line (gv/vec2) q)
  (q/text-size 16)
  (q/fill 0)
  (let [theta (g/heading q)
        num (scs/cl-format "~1$" theta)
        [x0 y0] (-> q
                    (g/scale 1.1)
                    (g/translate (gv/vec2 (* -0.5 (q/text-width num)) 6)))]
    (q/text-num num x0 y0)))

(defn debug-chain [chain]
  (let [segments
        (->> chain :segments (map (juxt :base :angle))
             (mapv (fn [[p a]]
                     {:p (mapv int p)
                      :rθ (debug/fixed-width a)
                      :θ (debug/fixed-width (g/heading p))
                      :d (int (g/dist (gv/vec2) p))})))
        turns (for [[a b c] (partition 3 1 segments)]
                (assoc b :dir (v/orientation (:p b) (:p a) (:p c))))]
    (concat (take 1 segments) turns (take-last 1 segments))))

(defn radial-points [points heading]
  (let [pts (geometry/radial-sort (gv/vec2) points)
        [b a] (split-with (fn [p] (<= (g/heading p) heading)) pts)]
    (concat a b)))

(defn draw [{:keys [radius mouse chain]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/translate (cq/rel-pos 0.5 0.5))
  (q/stroke 0)
  (draw-unit radius)
  (let [axis [(gv/vec2 0 radius)
              (gv/vec2 0 (- radius))
              (gv/vec2 radius 0)
              (gv/vec2 (- radius) 0)]
        quarter-axis (map #(g/rotate % (- (/ Math/PI 4))) axis)]
    (doseq [p axis]
      (draw-bisector p 0.5))
    (doseq [p quarter-axis]
      (draw-bisector p 0.3))

    (let [[mx my] mouse
          [tx ty] (tm/- (v/polar (* radius 1.5) 0.8) (gv/vec2 8 0))]
      (q/text (scs/cl-format "~1,1$ [~1,3$ ~1,3$]" (g/heading mouse) mx my)
              tx ty)
      (q/stroke 0 0.5 0.5)
      (q/stroke-weight 1.0)
      (q/no-fill)
      (cq/draw-path (g/vertices chain)))

    (let [axis-pts (radial-points (concat axis quarter-axis) (g/heading mouse))]
      (q/no-stroke)
      (doseq [[i [p q]] (map-indexed vector (partition 2 1 axis-pts))]
        (q/fill 0.6 0.5 0.5 (* 0.4 (/ (inc i) (inc (count axis-pts)))))
        (q/arc 0 0 radius radius (g/heading p) (g/heading q) :pie))
      (q/no-fill)

      (swap! defo assoc
             :mouse {:p mouse
                     :heading (debug/fixed-width (g/heading mouse))
                     :atan2 (debug/fixed-width (poly-detect/atan2 mouse))
                     :small-angle (debug/fixed-width (poly-detect/small-angle-between mouse (gv/vec2 1 0)))}
             :axis {:cw (first axis-pts) :ccw (last axis-pts)}
             :chain (debug-chain chain)))))

(sketch/defquil unit-circle
  :created-at "2021-10-28"
  :tags #{:demo}
  :on-mount (debug/mount defo)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
