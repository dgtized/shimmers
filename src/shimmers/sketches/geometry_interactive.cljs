(ns shimmers.sketches.geometry-interactive
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state {}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:position (gv/vec2)})

(defn update-state [state]
  (update state :position cq/mouse-last-position-clicked))

(defn draw [{:keys [position]}]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke 0.0)
  (let [a (cq/screen-rect 0.4)
        b (assoc (cq/screen-rect 0.3) :p position)
        diffs (square/difference a b)
        intersect (tm/intersection a b)
        union (tm/union a b)]
    (qdg/draw a)
    (qdg/draw b)
    (q/stroke 0.0 0.5 0.5)
    (q/fill 0.0 0.1)
    (doseq [s diffs]
      (qdg/draw s))

    (q/stroke 0.33 0.5 0.5)
    (q/fill 0.0 0.33)
    (when intersect
      (qdg/draw intersect))

    (q/fill 0.0 0.05)
    (q/stroke 0.0 0.1)
    (qdg/draw union)
    (swap! defo assoc
           :differences diffs
           :union union
           :intersection intersect)))

(defn ui-controls []
  [:div
   [:p.readable-width "Interactive test for computing rectangle differences
    between two rectangles. Left click with the mouse will place the upper left
    corner of the second rectangle and show the resulting rectangles in the
    difference, union and intersection."]
   (debug/display defo)])

(defn page []
  [:div (sketch/component
         :size [800 600]
         :setup setup
         :update update-state
         :draw draw
         :middleware [m/fun-mode framerate/mode])
   [:div.contained
    [ui-controls]]])

(sketch/definition geometry-interactive
  {:created-at "2023-02-16"
   :tags #{:demo}
   :type :quil}
  (ctrl/mount page "sketch-host"))
