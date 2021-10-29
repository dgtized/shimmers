(ns shimmers.sketches.unit-circle
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
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

(defn draw-bisector [{[p q] :points} weight]
  (q/stroke-weight weight)
  (q/line p q)
  (q/text-size 16)
  (q/fill 0)
  (let [theta (g/heading q)
        num (tm/roundto theta 0.01)
        [x0 y0] (-> q
                    (g/scale 1.1)
                    (g/translate (gv/vec2 (* -0.5 (q/text-width num)) 6)))]
    (q/text-num num x0 y0)))

(defn draw [{:keys [radius mouse chain]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/translate (cq/rel-pos 0.5 0.5))
  (q/stroke 0)
  (draw-unit radius)
  (let [axis [(gl/line2 (gv/vec2) [0 radius])
              (gl/line2 (gv/vec2) [0 (- radius)])
              (gl/line2 (gv/vec2) [radius 0])
              (gl/line2 (gv/vec2) [(- radius) 0])]
        quarter-axis (map #(g/rotate % (- (/ Math/PI 4))) axis)]
    (doseq [line axis]
      (draw-bisector line 0.5))
    (doseq [line quarter-axis]
      (draw-bisector line 0.3))

    (let [[x y] (v/polar (* radius 1.5) 0.8)
          num (tm/roundto (g/heading mouse) 0.01)]
      (q/text (str num " " (map #(tm/roundto % 0.1) mouse)) x y)
      (q/stroke 0 0.5 0.5)
      (q/stroke-weight 1.0)
      (q/no-fill)
      (cq/draw-path (g/vertices chain)))

    (let [pchain
          (->> chain :segments (map (juxt :base :angle))
               (mapv (fn [[p a]]
                       {:p (mapv int p)
                        :rθ (tm/roundto a 0.01)
                        :θ (tm/roundto (g/heading p) 0.01)
                        :d (int (g/dist (gv/vec2) p))
                        })))
          turns (for [[a b c] (partition 3 1 pchain)]
                  (assoc b :dir (v/orientation (:p b) (:p a) (:p c))))]
      (swap! defo assoc :chain (concat (take 1 pchain) turns (take-last 1 pchain))))))

(sketch/defquil unit-circle
  :created-at "2021-10-28"
  :tags #{:demo}
  :on-mount (fn [] (debug/mount defo))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
