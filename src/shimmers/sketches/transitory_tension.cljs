(ns shimmers.sketches.transitory-tension
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:screen-size "900x600"}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:dt 0.0
   :t (/ (q/millis) 1000.0)})

(defn update-state [{:keys [t] :as state}]
  (let [dt (- (/ (q/millis) 1000.0) t)]
    (-> state
        (assoc :dt dt)
        (update :t + dt))))

(defn draw [{:keys [t]}]
  (q/stroke (dr/gaussian (/ (mod t 5.5) 5.5) 0.01)
            (dr/gaussian 0.5 0.1)
            (dr/gaussian 0.6 0.1)
            1.0)
  (q/stroke-weight (tm/clamp (dr/gaussian 1.5 0.66) 0.2 100.0))
  (let [top (cq/rel-h (+ (* 0.1 (eq/unit-sin (* tm/PHI t)))
                         (* 0.1 (eq/unit-cos (/ t tm/PHI)))))
        bottom (- (q/height) top)]
    (let [x (cq/rel-w (/ (mod t 5.0) 5.0))]
      (q/line (+ x (dr/gaussian 0.0 4.0)) top
              (+ x (dr/gaussian 0.0 4.0)) bottom))
    (q/stroke (tm/smoothstep* 0.2 0.55 (eq/unit-cos (* 0.65 tm/PHI t))))
    (let [x (cq/rel-w (/ (mod (- t) 13.0) 13.0))]
      (q/line (+ x (dr/gaussian 0.0 2.0)) top
              (+ x (dr/gaussian 0.0 2.0)) bottom))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.flexcols
    [:div.readable-width
     [:p "Draw colored, vertical lines from left to right shifting the hue.
     Then, from right to left draw grayscale lines to erase."]]
    [ctrl/container
     [ctrl/dropdown ui-state "Screen Size" [:screen-size]
      (screen/sizes)
      {:on-change #(view-sketch/restart-sketch :transitory-tension)}]]]])

(sketch/definition transitory-tension
  {:created-at "2024-01-10"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
