(ns shimmers.sketches.trigonometry-boxes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.vector :as gv]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.quil :as cq]))

(defn box [center width height modulations t]
  (let [{:keys [center width height]}
        ((apply comp modulations) {:center center :width width :height height} t)
        wh (gv/vec2 (* width 0.5) (* height 0.5))]
    (rect/rect (tm/- center wh) (tm/+ center wh))))

(defn slide [v dt t0]
  (fn [box t]
    (update box :center tm/+ (tm/* v (Math/sin (+ t0 (* t dt)))))))

(defn resize [[dx dy] dt t0]
  (fn [box t]
    (-> box
        (update :width + (* dx (Math/sin (+ t0 (* t dt)))))
        (update :height + (* dy (Math/sin (+ t0 (* t dt))))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (/ (q/millis) 1000.0)
   :boxes [(partial box (cq/rel-vec 0.3 0.5) (cq/rel-w 0.2) (cq/rel-h 0.1)
                    [(resize (cq/rel-vec 0.2 0.0) 1.0 0.0)])
           (partial box (cq/rel-vec 0.5 0.5) (cq/rel-w 0.3) (cq/rel-h 0.2)
                    [(slide (cq/rel-vec 0.15 0) 0.8 0.0)])
           (partial box (cq/rel-vec 0.7 0.5) (cq/rel-w 0.2) (cq/rel-h 0.1)
                    [(resize (cq/rel-vec 0.0 0.15) 0.9 0.1)])]})

(defn update-state [{:keys [t] :as state}]
  (assoc state :t (/ (q/millis) 1000.0)))

(defn update-box [t box]
  (box t))

(defn draw [{:keys [boxes t]}]
  (q/background 1.0)
  (q/stroke 0.0 0.5)
  (q/fill 0.5 0.1)
  (doseq [box boxes]
    (qdg/draw (update-box t box))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition trigonometry-boxes
  {:created-at "2023-12-30"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
