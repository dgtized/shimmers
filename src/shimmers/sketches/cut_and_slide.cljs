(ns shimmers.sketches.cut-and-slide
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defn cutting-line []
  (let [[a b c d] (g/edges (cq/screen-rect 1.0))
        [[p1 q1] [p2 q2]] (dr/rand-nth [[a c] [b d] [c a] [d b]])]
    (gl/line2 (tm/mix p1 q1 (dr/random 0.1 0.9))
              (tm/mix p2 q2 (dr/random 0.1 0.9)))))

(defn slide [line force shape]
  (let [{[p q] :points} line
        slope (tm/* (tm/- q p) force)]
    (if (> (g/classify-point line (g/centroid shape)) 0)
      (g/translate shape slope)
      shape)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:action :init})

(defn update-state [{:keys [shapes lines action force time] :as state}]
  (case action
    :init
    {:shapes [(cq/screen-rect 0.66)]
     :lines []
     :action :cut
     :time 0}
    :cut
    (let [line (cutting-line)]
      (if (> (count shapes) 800)
        (assoc state :action :init)
        (assoc state
               :shapes (mapcat (fn [s] (lines/cut-polygon s line)) shapes)
               :lines (conj lines line)
               :time (q/frame-count)
               :force (/ 1 (dr/random 3000 15000))
               :action :slide)))
    :slide
    (if (> (- (q/frame-count) time) 100)
      (assoc state
             :time (q/frame-count)
             :action :cut)
      (assoc state
             :shapes (map (partial slide (last lines) force) shapes)))))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (doseq [s shapes]
    (cq/draw-polygon s)))

(sketch/defquil cut-and-slide
  :created-at "2022-09-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
