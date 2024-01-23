(ns shimmers.sketches.harmonograph
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:table [1 1]
               :pendulum [1.001 3]
               :pen [1 2]
               :pen-phase [1 3]
               :dampen-rate 0.15
               :dampen-limit 0.2
               :modulate-stroke true
               :pen-modulation false}))

(defn ui-controls []
  (ctrl/container
   [:h3 "Parameters"]
   [:div.flexcols
    [:div {:style {:width "8em"}} "Table Ratio"]
    (ctrl/numeric ui-state "N" [:table 0] [0 32 0.001])
    (ctrl/numeric ui-state "D" [:table 1] [1 16 0.1])]
   [:div.flexcols
    [:div {:style {:width "8em"}} "Pendulum Ratio"]
    (ctrl/numeric ui-state "N" [:pendulum 0] [0 32 0.001])
    (ctrl/numeric ui-state "D" [:pendulum 1] [1 16 0.1])]
   [:div {:style {:width "16em"}}
    (ctrl/numeric ui-state "Dampen Rate" [:dampen-rate] [0.01 0.5 0.01])
    (ctrl/numeric ui-state "Dampen Limit" [:dampen-limit] [0.01 0.2 0.01])
    (ctrl/checkbox ui-state "Modulate Stroke" [:modulate-stroke])
    (ctrl/checkbox ui-state "Pen Modulation" [:pen-modulation])]
   (when (:pen-modulation @ui-state)
     [:<>
      [:div.flexcols
       [:div {:style {:width "8em"}} "Pen Stroke Ratio"]
       (ctrl/numeric ui-state "N" [:pen 0] [0 32 0.001])
       (ctrl/numeric ui-state "D" [:pen 1] [1 16 0.1])]
      [:div.flexcols
       [:div {:style {:width "8em"}} "Pen Phase Rate"]
       (ctrl/numeric ui-state "N" [:pen-phase 0] [0 32 0.001])
       (ctrl/numeric ui-state "D" [:pen-phase 1] [1 16 0.1])]])

   [:em "(updates after restart)"]))

(defn dampen [lambda t]
  (Math/exp (* (- lambda) t)))

(defn ratio [[a b]]
  (/ (float a) b))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [table pendulum pen pen-phase]} @ui-state]
    (merge @ui-state
           {:t 0
            :dplat (ratio table)
            :dpend (ratio pendulum)
            :dpen (ratio pen)
            :dpen-phase (ratio pen-phase)})))

(defn update-state [state]
  (update state :t + 1))

(defn modular-stroke [t]
  (q/stroke-weight (+ 0.6 (* 0.4 (Math/sin (* 2 t))))))

(defn skip-draw [k t]
  (when (> (Math/sin (+ (* (/ 7 5) t) (* 3 (Math/sin (* (/ 1 5) t))))) 0)
    (modular-stroke t)
    (apply q/point (v/polar (* 0.3 (q/height) k) (* (/ 1 6) t)))))

(defn draw
  [{:keys [t dplat dpend dampen-rate dampen-limit modulate-stroke
           pen-modulation dpen dpen-phase]}]
  (q/stroke-weight 0.33)
  (dotimes [i 1000]
    (let [t (+ (* 4.0 t) (/ i 200))
          k (dampen dampen-rate (* 0.01 t))]
      (if (< k dampen-limit)
        (q/no-loop)
        (q/with-translation
            [(tm/+ (cq/rel-vec 0.5 0.5)
                   (gv/vec2 (* 0.225 (q/height) k (Math/cos (* 1 dplat t)))
                            (* 0.225 (q/height) k (Math/sin (* 1 dplat t)))))]
          (when modulate-stroke
            (modular-stroke t))
          (when (or (not pen-modulation)
                    (> (Math/sin (+ (* dpen t) (* 2 (Math/sin (* dpen-phase t))))) 0))
            (apply q/point
                   (gv/vec2 (* 0.225 (q/height) k (Math/cos (* 1 dpend t)))
                            (* 0.225 (q/height) k (Math/sin (* 1 dpend t)))))))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ui-controls]])

(sketch/definition harmonograph
  {:created-at "2024-01-22"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
