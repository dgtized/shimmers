(ns shimmers.sketches.vanishing-points
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds (cq/screen-rect)
   :mouse (gv/vec2)})

(defn update-state [{:keys [bounds] :as state}]
  (assoc state :mouse (v/clamp-bounds bounds (cq/mouse-position))))

(defn draw [{:keys [bounds mouse] :as state}]
  (q/background 1.0 0.1)
  (q/fill 1.0 0.1)
  (reset! defo state)
  (let [outer (g/scale-size bounds 0.8)
        inner (g/scale bounds 0.15)
        bounded-mouse (v/clamp-bounds (rect/rect (:p outer)
                                                 (tm/- (:size outer)
                                                       ;; Why 1/3, only works for current scale
                                                       (tm/* (:size inner) 0.33)))
                                      mouse)
        cursor (g/translate inner bounded-mouse)]
    (cq/draw-polygon outer)
    (let [vertex-pairs (map vector (g/vertices outer) (g/vertices cursor))
          divisions (partition 2 1 (conj vertex-pairs (last vertex-pairs)))]
      (doseq [[p q] vertex-pairs]
        (q/line p q))
      (doseq [t (tm/norm-range 5)]
        (doseq [[[p1 q1] [p2 q2]] divisions]
          (q/line (tm/mix p1 q1 t) (tm/mix p2 q2 t)))))
    (cq/draw-polygon cursor)))

(sketch/defquil vanishing-points
  :created-at "2022-03-05"
  :on-mount (fn [] (debug/mount defo))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
