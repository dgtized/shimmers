(ns shimmers.sketches.cut-and-slide
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defn random-color []
  [(dr/random)
   (dr/random 0.2 0.6)
   (dr/random 0.25 0.75)
   (dr/random 0.25 0.75)])

(defn cutting-line [angle]
  (let [start (g/random-point-inside (cq/screen-rect 0.75))
        dir (v/polar 1000 angle)]
    (first (lines/clip-line (gl/line2 (tm/- start dir) (tm/+ start dir))
                            (cq/screen-rect)))))

(defn displacement-force [angle]
  (let [force (/ 1 (dr/random 4 32))]
    (v/polar force (if (dr/chance 0.2)
                     (+ angle tm/HALF_PI)
                     angle))))

(defn cut [line shape]
  (let [polygons (lines/cut-polygon shape line)
        color (:color shape)]
    (map #(assoc % :color (if (> (count polygons) 1)
                            (color/mixer color (random-color) 0.2)
                            color))
         polygons)))

(defn slide [line force shape]
  (if (> (g/classify-point line (g/centroid shape)) 0)
    (assoc (g/translate shape force) :color (:color shape))
    shape))

(defonce ui-state (ctrl/state {:show-colors true}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:action :init})

(defn update-state [{:keys [shapes angle lines action force time] :as state}]
  (case action
    :init
    {:shapes [(assoc (cq/screen-rect 0.66) :color (random-color))]
     :lines []
     :angle (dr/random eq/TAU)
     :action :cut
     :time 0}
    :cut
    (let [line (cutting-line angle)]
      (if (> (count shapes) 800)
        (assoc state :action :init)
        (assoc state
               :shapes (mapcat (partial cut line) shapes)
               :lines (conj lines line)
               :angle (+ (* angle tm/PHI) (dr/random 0.1))
               :time (q/frame-count)
               :force (displacement-force angle)
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
  (q/fill 1.0)
  (let [{:keys [show-colors]} @ui-state]
    (doseq [s shapes]
      (when-let [color (and show-colors (:color s))]
        (apply q/fill color))
      (cq/draw-polygon s))))

(defn ui-controls []
  [:div
   (ctrl/checkbox ui-state "Show Colors" [:show-colors])])

(sketch/defquil cut-and-slide
  :created-at "2022-09-05"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
