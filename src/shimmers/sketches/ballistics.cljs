(ns shimmers.sketches.ballistics
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]))

(defrecord turret [pos dir])
(defrecord ballistic [pos vel mass])
(defrecord missile [pos vel mass fuel])

(defn setup []
  (q/noise-seed (dr/random-int 100000))
  (q/color-mode :hsl 1.0)
  {:ground
   (->> (concat [0.0] (dr/gaussian-range 0.03 0.01) [1.0])
        (mapv (fn [x] (cq/rel-vec x (- 1.0 (* 0.4 (q/noise (* 4 x) 0.5))))))
        gl/linestrip2)})

(defn update-state [state]
  state)

(defn draw [{:keys [ground]}]
  (q/background 1.0)

  (let [verts (g/vertices ground)]
    (cq/draw-curve-path
     (concat [(gv/vec2 (- 0.0 (* (q/width) 0.05)) (:y (first verts)))]
             verts
             [(gv/vec2 (* (q/width) 1.05) (:y (last verts)))]))))

(sketch/defquil ballistics
  :created-at "2023-02-05"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
