(ns shimmers.sketches.clothoid-flowers
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/noise-seed (dr/seed))
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (q/ellipse-mode :radius)
  (q/no-stroke)
  (q/fill 0.0 0.3)

  (q/translate (cq/rel-vec 0.5 0.5))
  (let [rotation (* eq/TAU (q/noise (* 0.01 t)))
        length (+ 40 (* 20 (Math/sin t)))]
    (->> (concat (eq/clothoid 18 length 30 -1 (+ rotation 0.0) (gv/vec2))
                 (eq/clothoid 12 length 50 -1 (+ rotation Math/PI) (gv/vec2)))
         (mapv #(tm/* % 12))
         (cq/plot (fn [p] (cq/circle p 0.3))))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition clothoid-flowers
  {:created-at "2021-11-23"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))
