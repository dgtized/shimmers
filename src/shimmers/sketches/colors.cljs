(ns shimmers.sketches.colors
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as r]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.core :as sm]))

;; TODO: add hover color info for hsl values?
;; Limit range to show spectrum / palette somehow?
(defn controls [ui]
  (fn []
    [:div
     (ctrl/change-mode ui [:hsla :mix-mod])
     (case (:mode @ui)
       :hsla [:div
              (ctrl/checkbox ui "Animate" [:animate])
              (ctrl/slider ui (fn [v] (str "Brightness: " (/ v 100)))
                           [:lightness] [0 100])
              (ctrl/slider ui (fn [v] (str "Opacity: " (/ v 100)))
                           [:alpha] [0 100])]
       :mix-mod [:div
                 (ctrl/slider ui (fn [v] (str "Hue1: " v))
                              [:hue1] [0 100])
                 (ctrl/slider ui (fn [v] (str "Hue2: " v))
                              [:hue2] [0 100])])]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [ui (r/atom {:mode :hsla :lightness 50 :alpha 100
                    :hue1 (int (* 100 (rand)))
                    :hue2 (int (* 100 (rand)))})]
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

(defn draw-mix-mod [ui]
  (let [dx 0.01
        [h1 h2] (map #(/ % 100) (vals (select-keys @ui [:hue1 :hue2])))
        s 0.5
        l 0.5
        a 1.0]
    (doseq [h (range 0 1 dx)]
      (q/fill (sm/mix-mod h1 h2 h) s l a)
      (q/rect (cq/rel-w h) (cq/rel-h 0) (cq/rel-w dx) (cq/rel-h 1)))))

(defn draw [{:keys [ui]}]
  (q/background 1.0)
  (q/no-stroke)
  (case (:mode @ui)
    :hsla (draw-hsla ui)
    :mix-mod (draw-mix-mod ui)))

(defn ^:export run-sketch []
  (q/defsketch colors
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
