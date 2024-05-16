(ns shimmers.sketches.kinematic-chain
  "Inspired by https://www.youtube.com/watch?v=hbgDqyy8bIwa"
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:mode :sin}))

(defn noise-target [rate bw bh]
  (let [t (q/millis)]
    (cq/rel-vec (q/noise bw (/ t rate))
                (q/noise bh (/ t rate)))))

(defn sin-target []
  (let [t (q/millis)]
    (cq/rel-vec (tm/map-interval (q/cos (+ tm/PI (/ t 10000))) [-1 1] [0.05 0.95])
                (tm/map-interval (q/sin (/ t 2000)) [-1 1] [0.1 0.9]))))

(def modes {:sin sin-target
            :mouse cq/mouse-position
            :noise (partial noise-target 4500 100 200)})

(defn follow []
  ((get modes (:mode @ui-state))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:chain (assoc (chain/make-chain (follow) 96 16)
                 :color [0.35 0.5 0.5 0.03])})

(defn update-state [state]
  (-> state
      (update :chain chain/chain-update nil (follow))
      (update-in [:chain :color 0] (fn [c] (mod (+ 0.001 c) 1.0)))))

(defn draw [{:keys [chain]}]
  (q/no-fill)
  ;; (q/background 255)
  (apply q/stroke (:color chain))
  (cq/draw-path (g/vertices chain)))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ctrl/change-mode ui-state (keys modes)]])

(sketch/definition kinematic-chain
  {:created-at "2021-03-19"
   :tags #{:demo}
   :type :quil}
  (ctrl/mount page))
