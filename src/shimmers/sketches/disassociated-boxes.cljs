(ns shimmers.sketches.disassociated-boxes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn shrink [scale shape]
  (geom/scale-size shape scale))

(defn displace [scale shape]
  (geom/translate shape (tm/* (gv/randvec2) (* scale (geom/area shape)))))

(comment (displace 0.01 (rect/rect 5 5 10 10)))

(def divisions
  {[2 1] 3 [1 2] 3
   [3 1] 2 [1 3] 2
   [4 1] 2 [1 4] 2
   [5 1] 2 [1 5] 2
   [6 1] 1 [1 6] 1
   [7 1] 1 [1 7] 1
   [2 3] 1 [3 2] 1
   [2 2] 1 [3 3] 1})

(defn disassociate [shape]
  (let [[hdivs vdivs] (p/weighted divisions)
        w (geom/width shape)
        hstride (/ w hdivs)
        h (geom/height shape)
        vstride (/ h vdivs)
        [x y] (:p shape)]
    (->> (for [ow (range 0 w hstride)
               ov (range 0 h vstride)]
           (rect/rect (+ x ow) (+ y ov) hstride vstride))
         (p/map-random-sample 0.05 (partial shrink (rand-nth [0.8 0.9 0.95])))
         (p/map-random-sample 0.05 (partial displace 0.001)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 0.95)]})

(defn update-state [state]
  (if (< (count (:shapes state)) 1000)
    (update state :shapes (partial p/mapcat-random-sample 0.2 disassociate))
    state))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (doseq [shape shapes]
    (cq/draw-shape (geom/vertices shape))))

(defn ^:export run-sketch []
  ;; 20210505
  (q/defsketch disassociated-boxes
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
