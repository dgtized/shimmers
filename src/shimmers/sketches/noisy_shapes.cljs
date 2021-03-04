(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as poly]
            [thi.ng.geom.rect :as rect]))

(defn setup []
  (let [w (q/width)
        h (q/height)]
    {:shape (rect/rect (* 0.2 w) (* 0.2 h) (* 0.5 w) (* 0.6 h))}))

(defn update-state [state]
  state)

(defn draw-polygon [poly]
  (q/begin-shape)
  (doseq [p (geom/vertices poly)]
    (apply q/vertex p))
  (q/end-shape :close))

(defn draw [{:keys [shape]}]
  (q/background 255)
  (let [w (q/width)
        h (q/height)
        poly (geom/as-polygon shape)]
    ;; (draw-polygon poly)
    (q/stroke-weight 0.1)
    (q/fill 0 0.5 0.8 0.1)
    (doseq [copy [(geom/translate poly (gv/randvec2 3))
                  (geom/translate poly (gv/randvec2 2))
                  (geom/translate (geom/rotate (geom/center poly (gv/vec2 0 0))
                                               (q/random -0.2 0.2))
                                  (geom/centroid poly))
                  ]]
      (draw-polygon copy))))

(defn ^:export run-sketch []
  (q/defsketch noisy-shapes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
