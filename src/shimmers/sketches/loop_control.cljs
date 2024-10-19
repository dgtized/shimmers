(ns shimmers.sketches.loop-control
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]
   [clojure.math :as math]))

(defn random-equation []
  (dr/weighted [[(fn [r t] (* t (* 8.0 (+ r 1)))) 1.0]
                [(fn [r t] (* t (math/exp (+ 1 r)))) 1.0]
                [(fn [r t] (* t 2.0 (math/pow 3 (+ 1 r)))) 1.0]
                [(fn [r t] (* t (+ 10.0 (math/sin (* eq/TAU r))))) 1.0]]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [equation (random-equation)]
    {:circles
     (into []
           (for [r (butlast (tm/norm-range 24))]
             {:p (v/+polar (cq/rel-vec 0.5 0.5) (cq/rel-h 0.38) (* r eq/TAU))
              :rate (partial equation r)}))}))

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (let [t (/ (q/millis) 1000.0)
        r (cq/rel-h 0.05)]
    (doseq [{:keys [p rate]} circles
            :let [p2 (v/+polar p r (rate t))]]
      (cq/circle p 2.0)
      (q/line p p2)
      (cq/circle p2 2.0))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition loop-control
  {:created-at "2024-10-19"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
