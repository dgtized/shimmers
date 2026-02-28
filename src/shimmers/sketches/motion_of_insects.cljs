(ns shimmers.sketches.motion-of-insects
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.verlet-particles :as vp]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn neighborhood [{at-p :pos :as p} particles radius]
  (filter (fn [{at-q :pos :as q}]
            (and (not= p q) (< (g/dist at-p at-q) radius)))
          particles))

(defn flock-separation [radius strength likelyhood]
  (fn [{:keys [particles]} {at-p :pos mass :mass :as p} delta]
    (if (dr/chance likelyhood)
      (let [neighborhood (neighborhood p particles (+ mass radius))]
        (when (seq neighborhood)
          (let [differences (map (fn [{at-q :pos}]
                                   (tm/div (tm/- at-p at-q) (g/dist at-p at-q)))
                                 neighborhood)
                rel-diff (tm/div (reduce tm/+ differences) (count neighborhood))]
            (tm/* rel-diff (* strength delta)))))
      (gv/vec2))))

(defn jumping [distance likelyhood]
  (fn [_ _ _]
    (if (dr/chance likelyhood)
      (dr/jitter distance)
      (gv/vec2))))

(defn make-insect []
  (let [p (cq/rel-vec (dr/random) (dr/random))]
    (vp/make-particle p (tm/+ p (dr/jitter 1.0)) (dr/random 1.0 3.0))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [fps 60
        n 128]
    {:system
     (vp/make-system {:particles (repeatedly n make-insect)
                      :mechanics [(flock-separation 36.0 2.0 (/ 4.0 fps))
                                  (jumping 24.0 (/ 2.0 (* fps n)))]
                      :constraints [(vp/wrap-around (q/width) (q/height))]
                      :drag 0.1})}))

(defn update-state [{:keys [system] :as state}]
  (vp/timestep system 2)
  state)

(defn draw [{:keys [system]}]
  (q/background 1.0 (tm/random 0.01 0.2))
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [pos mass]} (:particles system)]
    (cq/circle pos (+ mass 4.0))))

(defn page []
  [:div (sketch/component
          :size [800 600]
          :setup setup
          :update update-state
          :draw draw
          :middleware [m/fun-mode framerate/mode])
   [:div.centered.readable-width
    [:p "After watching some water spiders skitter across the surface of a
    river, attempted to mimic how they kept an even spacing even as they
    constantly adjusted their position."]]])

(sketch/definition motion-of-insects
  {:created-at "2021-09-13"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))
