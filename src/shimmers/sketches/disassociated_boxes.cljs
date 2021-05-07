(ns shimmers.sketches.disassociated-boxes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.color :as color]
            [shimmers.math.probability :as p]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn shrink [scale shape]
  (geom/scale-size shape scale))

(defn displace [scale shape]
  (geom/translate shape (tm/* (gv/randvec2) (* scale (geom/area shape)))))

;; https://artsexperiments.withgoogle.com/artpalette/colors/3a3737-a25547-a19382-c9b9a5-ece7e1
(def palette1 ["#3a3737" "#a25547" "#a19382" "#c9b9a5" "#ece7e1"])
;; https://artsexperiments.withgoogle.com/artpalette/colors/7085ad-d0d2c8-556895-969796-8fa4c3
(def palette2 ["#7085ad" "#d0d2c8" "#556895" "#969796" "#8fa4c3"])

(defn colorize [palette shape]
  (assoc shape :color (color/hex->hsla (rand-nth palette))))

(comment (colorize palette1 (displace 0.01 (rect/rect 5 5 10 10))))

(def divisions
  {[2 1] 3 [1 2] 3
   [3 1] 3 [1 3] 3
   [4 1] 1 [1 4] 1
   [5 1] 3 [1 5] 3
   [6 1] 1 [1 6] 1
   [7 1] 2 [1 7] 2
   [3 2] 1 [2 3] 1
   [2 2] 1 [3 3] 1})

(defn split-bias
  "Generates divisions and biases the weights towards row splits or column splits."
  [row-bias]
  (reduce-kv (fn [m [r c] v]
               (let [bias (cond (> r c) row-bias
                                (< r c) (/ 1.0 row-bias)
                                :else 1.0)]
                 (assoc m [r c] (* bias v))))
             {}
             divisions))

(defn disassociate [palette shape]
  (let [w (geom/width shape)
        h (geom/height shape)
        ;; bias towards column or row centric splits based on a weighted ratio
        ;; of current shapes width / height
        [hdivs vdivs] (->> (/ (+ 2.0 (/ w h)) 3)
                           split-bias
                           p/weighted)
        hstride (/ w hdivs)
        vstride (/ h vdivs)
        [x y] (:p shape)]
    (->> (for [ow (range 0 w hstride)
               ov (range 0 h vstride)]
           (rect/rect (+ x ow) (+ y ov) hstride vstride))
         (p/map-random-sample 0.05 (partial shrink (rand-nth [0.7 0.8 0.9 0.95])))
         (p/map-random-sample 0.05 (partial displace 0.002))
         (p/map-random-sample 0.10 (partial colorize palette)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:palette (rand-nth [palette1 palette2])
   :shapes [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 0.95)]})

(defn update-state [state]
  (if (< (count (:shapes state)) 1000)
    (update state :shapes (partial p/mapcat-random-sample 0.2 (partial disassociate (:palette state))))
    state))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (doseq [shape shapes]
    (if-let [c (:color shape)]
      (apply q/fill c)
      (q/no-fill))
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
