(ns shimmers.sketches.cut-and-slide
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defn cutting-line [angle]
  (let [start (g/random-point-inside (cq/screen-rect 0.66))
        dir (v/polar 1000 angle)]
    (first (lines/clip-line (gl/line2 (tm/- start dir) (tm/+ start dir))
                            (cq/screen-rect)))))

(defn displacement-force [line]
  (let [{[p q] :points} line
        force (/ 1 (dr/random 3000 15000))]
    (tm/* (tm/- q p) force)))

(defn slide [line force shape]
  (if (> (g/classify-point line (g/centroid shape)) 0)
    (g/translate shape force)
    shape))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:action :init})

(defn update-state [{:keys [shapes angle lines action force time] :as state}]
  (case action
    :init
    {:shapes [(cq/screen-rect 0.66)]
     :lines []
     :angle (dr/random eq/TAU)
     :action :cut
     :time 0}
    :cut
    (let [line (cutting-line angle)]
      (if (> (count shapes) 800)
        (assoc state :action :init)
        (assoc state
               :shapes (mapcat (fn [s] (lines/cut-polygon s line)) shapes)
               :lines (conj lines line)
               :angle (+ (* angle tm/PHI) (dr/random 0.1))
               :time (q/frame-count)
               :force (displacement-force line)
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
