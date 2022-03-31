(ns shimmers.sketches.reagent-quil-component
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]))

(defn setup [speed]
  (fn [args]
    (println "setup" args)
    (q/color-mode :hsl 1.0)
    {:t 0
     :speed speed}))

(defn update-state [state]
  (update state :t + (:speed state)))

(defn draw [{:keys [t]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0)
  (when (<= (mod t 1) 0.0001)
    (println t))
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (cq/circle (v/polar (cq/rel-h 0.4) t) 20))

;; TODO: per sketch framerate, currently they are both updating the same value
;; TODO: adjusting width/height dynamically?

;; note that sketch/component is a macro specifically to allow repl changes to
;; setup/update/draw. Otherwise using shimmers.common.ui.quil/sketch-component
;; is better if defining each component programatically.
(defn page []
  [:div
   [:p.explanation.readable-width
    "Experimenting with wrapping Quil sketches in a Reagent Component"]
   (sketch/component
    :size [800 300]
    :setup (setup 0.01)
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p.explanation.readable-width
    "and now the same sketch, but with a setup function using dθ of 0.05"]
   (sketch/component
    :size [800 300]
    :setup (setup 0.05)
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p.explanation.readable-width
    "The sketches are nameless and mostly statically defined, as the macro "
    [:code "sketch/component"] " wraps the underlying "
    [:code "update"] " and " [:code "draw"] " routines, allowing live updates of
    those functions to propagate to the corresponding view."]])

(sketch/definition reagent-quil-component
  {:created-at "2022-03-29"
   :type :quil
   :tags #{}}
  (ctrl/mount page "sketch-host"))
