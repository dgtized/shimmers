(ns shimmers.sketches.circular-repetition
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.strf.core :as f]))

(defonce defo (debug/state {}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.25)
  ;; pseudo work-around as chrome is not drawing p3d lines since update sometime
  ;; in june 2024?
  (q/stroke 0.0)
  (q/fill 0.5 0.05)
  (let [params
        {:weights (repeatedly 3 #(dr/random -1.0 1.0))
         :osc (repeatedly 3 #(dr/random -0.5 0.5))
         :phase (repeatedly 3 dr/random-tau)}]
    (reset! defo params)
    params))

(defn draw [{[w1 w2 w3] :weights [o1 o2 o3] :osc [p1 p2 p3] :phase}]
  (q/background 1.0)
  (let [r (cq/rel-h 0.4)
        t (/ (q/millis) 2000.0)]
    (dotimes [i 360]
      (q/push-matrix)
      (let [angle i
            scale 0.075]
        (q/rotate-z (+ (* scale w3 (+ t angle)) (* 2 (math/sin (+ p3 (* o3 t))))))
        (q/rotate-y (+ (* scale w2 (+ t angle)) (* 2 (math/sin (+ p2 (* o2 t))))))
        (q/rotate-x (+ (* scale w1 (+ t angle)) (* 2 (math/sin (+ p1 (* o1 t)))))))
      (q/ellipse 0 0 r r)
      (q/pop-matrix))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :renderer :p3d
     :setup setup
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div
    [:div.readable-width
     "Inpired by " [:a {:href "https://junkiyoshi.com/openframeworks20240527/"} "Junki Yoshi - overlap circles"]
     " but using random phase modulation instead of pure noise to seed the x,y,z rotation of each circle."]
    [:div
     [:p]
     (let [{:keys [weights osc phase]} @defo]
       (for [[axis w o p] (mapv vector [:x :y :z] weights osc phase)]
         [:div {:key axis}
          [:code
           (f/format [(f/pad-left 2 " ") " "
                      (f/float 2) " " (f/float 2) " " (f/float 2)]
                     (name axis) w o p)]]))]]])

(sketch/definition circular-repetition
    {:created-at "2024-05-31"
     :tags #{}
     :type :quil}
  (ctrl/mount page))
