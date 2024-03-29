(ns shimmers.sketches.random-walk
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.particle-system :as particles]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-particle []
  (let [initial-pos (cq/rel-vec (rand) (rand))]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (gv/vec2 (q/random-2d))
     :acceleration (gv/vec2 (q/random-2d))
     :color (color/random)}))

(defn constrain2d [[x y] lower upper]
  (gv/vec2 (tm/clamp x lower upper)
           (tm/clamp y lower upper)))

(defn update-particle
  [{:keys [position velocity acceleration] :as particle}]
  (let [new-velocity (-> (v/add velocity acceleration) (constrain2d -1.5 1.5))
        new-position (v/add position new-velocity)
        wrapped-position (v/wrap2d new-position (q/width) (q/height))]
    (assoc particle
           :last-pos (if (= wrapped-position new-position) position wrapped-position)
           :position wrapped-position
           :velocity new-velocity
           :acceleration (g/scale (gv/vec2 (q/random-2d)) 0.5))))

(defn setup []
  (q/background "white")
  {:particles (repeatedly 50 make-particle)})

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw [{:keys [particles]}]
  ;; (q/background 256 16)
  (particles/draw particles))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition random-walk
  {:created-at "2020-10-21"
   :tags #{}
   :type :quil}
  (ctrl/mount page))


