(ns shimmers.sketches.colors
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]
   [shimmers.math.color :as color]
   [shimmers.math.equations :as eq]))

(defn random-hues []
  {:hue1 (int (* 100 (rand)))
   :hue2 (int (* 100 (rand)))})

;; TODO: add hover color info for hsl values?
;; Limit range to show spectrum / palette somehow?
(defn ui-controls [ui]
  (fn []
    (ctrl/container
     (ctrl/change-mode ui [:hsla :mix-mod :noise :oklab])
     (case (:mode @ui)
       :hsla
       [:div
        (ctrl/checkbox ui "Animate" [:animate])
        (ctrl/slider ui (fn [v] (str "Brightness: " (/ v 100)))
                     [:lightness] [0 100])
        (ctrl/slider ui (fn [v] (str "Opacity: " (/ v 100)))
                     [:alpha] [0 100])]

       :mix-mod
       [:div
        (ctrl/slider ui (fn [v] (str "Hue1: " v))
                     [:hue1] [0 100])
        (ctrl/slider ui (fn [v] (str "Hue2: " v))
                     [:hue2] [0 100])
        [:input {:type "button" :value "Randomize Hues"
                 :on-click #(swap! ui merge (random-hues))}]
        (ctrl/slider ui (fn [v] (str "Saturation1: " v))
                     [:saturation1] [0 100])
        (ctrl/slider ui (fn [v] (str "Saturation2: " v))
                     [:saturation2] [0 100])
        (ctrl/slider ui (fn [v] (str "Lightness1: " v))
                     [:lightness1] [0 100])
        (ctrl/slider ui (fn [v] (str "Lightness2: " v))
                     [:lightness2] [0 100])]
       :noise
       [:div
        (ctrl/numeric ui "Noise Multiplier" [:noise-mult] [0 16 0.00001])
        (ctrl/checkbox ui "Animate" [:animate])]
       :oklab
       [:div]))))

(defn setup []
  (let [defaults
        {:mode :hsla
         :lightness 50
         :alpha 100
         :saturation1 50
         :saturation2 50
         :lightness1 50
         :lightness2 50
         :noise-mult 0.5}
        ui (ctrl/state (merge defaults (random-hues)))]
    (ctrl/mount (ui-controls ui))
    {:ui ui}))

(defn update-state [state]
  state)

(defn draw-hsla [ui]
  (q/color-mode :hsl 1.0)
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
  (q/color-mode :hsl 1.0)
  (let [dx 0.01
        [h1 h2] (map #(/ % 100) (vals (select-keys @ui [:hue1 :hue2])))
        [s1 s2] (map #(/ % 100) (vals (select-keys @ui [:saturation1 :saturation2])))
        [l1 l2] (map #(/ % 100) (vals (select-keys @ui [:lightness1 :lightness2])))
        a 1.0]
    (doseq [h (range 0 1 dx)]
      (q/fill (sm/mix-mod h1 h2 h) (tm/mix* s1 s2 h) (tm/mix* l1 l2 h) a)
      (q/rect (cq/rel-w h) (cq/rel-h 0) (cq/rel-w dx) (cq/rel-h 1)))))

(defn draw-noise [ui]
  (q/color-mode :hsl 1.0)
  (let [{:keys [animate] m :noise-mult} @ui
        dx 0.01
        dy 0.01
        fc (q/frame-count)]
    (doseq [y (range 0 1 dy)]
      (doseq [x (range 0 1 dx)]
        (let [n (q/noise (* x m) (* y m) (if animate (* fc m 0.01) 0))]
          (q/fill n 0.5 0.5 1.0)
          (q/rect (cq/rel-w x) (cq/rel-h y) (cq/rel-w dx) (cq/rel-h dy)))))))

(defn draw-oklab [_]
  (q/color-mode :rgb 1.0)
  (let [dx 0.01
        dy 0.01]
    (doseq [y (range 0 1 dy)]
      (doseq [x (range 0 1 dx)]
        (let [[r g b] (color/oklab-lch (- 0.95 (* 0.4 y)) 0.15 (* eq/TAU x))]
          (q/fill r g b 1.0)
          (q/rect (cq/rel-w x) (cq/rel-h y) (cq/rel-w dx) (cq/rel-h dy)))))))

(defn draw [{:keys [ui]}]
  (q/background 1.0)
  (q/no-stroke)
  (case (:mode @ui)
    :hsla (draw-hsla ui)
    :mix-mod (draw-mix-mod ui)
    :noise (draw-noise ui)
    :oklab (draw-oklab ui)))

(sketch/defquil colors
  :created-at "2021-03-11"
  :tags #{:demo}
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
