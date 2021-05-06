(ns shimmers.sketches.shattered-boxes
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

(defn shatter [shape]
  (let [divs (+ 2 (rand-int 4))
        w (geom/width shape)
        h (geom/height shape)
        [x y] (:p shape)
        boxes (if (p/chance 0.5)
                (let [stride (/ w divs)]
                  (for [o (range 0 w stride)]
                    (rect/rect (+ x o) y stride h)))
                (let [stride (/ h divs)]
                  (for [o (range 0 h stride)]
                    (rect/rect x (+ y o) w stride))))]
    (->> boxes
         (p/map-random-sample 0.05 (partial shrink (rand-nth [0.8 0.9 0.95])))
         (p/map-random-sample 0.05 (partial displace 0.001)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 0.95)]})

(defn update-state [state]
  (if (< (count (:shapes state)) 1200)
    (update state :shapes (partial p/mapcat-random-sample 0.2 shatter))
    state))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (doseq [shape shapes]
    (cq/draw-shape (geom/vertices shape))))

(defn ^:export run-sketch []
  ;; 20210505
  (q/defsketch shattered-boxes
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
