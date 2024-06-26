(ns shimmers.sketches.concentric-moire
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/frame-rate 20)
  (q/color-mode :hsl 1.0)
  {:circles [{:pos (cq/rel-pos 0.2 0.5)
              :spacing 20 :upper 48 :weight 1.0}
             {:pos (cq/rel-pos 0.8 0.5)
              :spacing 20 :upper 48 :weight 1.0}
             {:pos (cq/rel-pos 0.5 0.25)
              :spacing 10 :upper 64 :weight 0.5}
             {:pos (cq/rel-pos 0.5 0.75)
              :spacing 10 :upper 64 :weight 0.5}]})

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/no-fill)
  (q/ellipse-mode :radius)
  (let [change (* 0.5 (+ 1 (math/sin (/ (q/frame-count) 40))))]
    (doseq [{:keys [pos spacing upper weight]} circles]
      (q/stroke-weight weight)
      (doseq [r (range 0 upper)]
        (cq/circle pos (* spacing (+ r change)))))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition concentric-moire
  {:created-at "2021-08-14"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
