(ns shimmers.sketches.colors
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [reagent.core :as r]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.sketches.lattice-of-common-chords :refer [color-mix]]))

;; TODO: add hover color info for hsl values?
;; Limit range to show spectrum / palette somehow?
(defn controls [ui]
  (fn []
    [:div
     (ctrl/change-mode ui [:hsla :nearby])
     (case (:mode @ui)
       :hsla [:div
              (ctrl/checkbox ui "Animate" [:animate])
              (ctrl/slider ui (fn [v] (str "Brightness: " (/ v 100)))
                           [:lightness] [0 100])
              (ctrl/slider ui (fn [v] (str "Opacity: " (/ v 100)))
                           [:alpha] [0 100])]
       :nearby [:div])]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [ui (r/atom {:mode :hsla :lightness 50 :alpha 100})]
    (ctrl/mount (controls ui))
    {:ui ui}))

(defn update-state [state]
  state)

(defn draw-hsla [ui]
  (let [{:keys [lightness alpha animate]} @ui
        fc (q/frame-count)
        dx 0.01
        dy 0.01
        l (/ lightness 100)
        a (/ alpha 100)]
    (doseq [h (range 0 1 dx)]
      (doseq [s (range 0 1 dy)]
        (q/fill (if animate (mod (+ h (/ fc 1000)) 1.0) h)
                s l a)
        (q/rect (cq/rel-w h) (cq/rel-h s) (cq/rel-w dx) (cq/rel-h dy))))))

(defn draw-nearby [ui]
  (let [dx 0.01
        s 0.5
        l 0.5
        a 1.0]
    (doseq [h (range 0 1 dx)]
      (q/fill h s l a)
      (q/rect (cq/rel-w h) (cq/rel-h 0) (cq/rel-w dx) (cq/rel-h 1)))))

(defn draw [{:keys [ui]}]
  (q/background 1.0)
  (q/no-stroke)
  (case (:mode @ui)
    :hsla (draw-hsla ui)
    :nearby (draw-nearby ui)))

(defn ^:export run-sketch []
  (q/defsketch colors
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
