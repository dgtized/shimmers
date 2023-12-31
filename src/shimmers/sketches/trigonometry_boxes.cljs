(ns shimmers.sketches.trigonometry-boxes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wave :as wave]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn box [center width height modulations t]
  (let [{:keys [center width height]}
        ((apply comp modulations) {:center center :width width :height height} t)
        wh (gv/vec2 (* width 0.5) (* height 0.5))]
    (rect/rect (tm/- center wh) (tm/+ center wh))))

(defn slide [v f dt t0]
  (fn [box t]
    (update box :center tm/+ (tm/* v (Math/sin (+ t0 (* t dt)))))))

(defn resize [[dx dy] f dt t0]
  (fn [box t]
    (-> box
        (update :width + (* dx (Math/sin (+ t0 (* t dt)))))
        (update :height + (* dy (Math/sin (+ t0 (* t dt))))))))

(defn gen-mod []
  (let [modf (dr/weighted {:slide 1.0 :resize 1.0})
        tf (dr/weighted [[Math/sin 3.0] [Math/cos 3.0] [Math/tan 1.0]
                         [(partial wave/triangle eq/TAU) 1.0]
                         [(partial wave/square eq/TAU) 1.0]
                         [(partial wave/sawtooth eq/TAU) 1.0]])
        dt (dr/random 0.5 1.5)
        t0 (dr/random-tau)]
    (({:slide slide :resize resize} modf)
     (apply cq/rel-vec
            (dr/weighted {[(dr/random 0.05 0.25) 0] 3.0
                          [0 (dr/random 0.05 0.25)] 3.0
                          [(dr/random 0.05 0.25)
                           (dr/random 0.05 0.25)] 1.0}))
     tf dt t0)))

(defn gen-box []
  (partial box
           (cq/rel-vec (dr/random 0.2 0.8)
                       (dr/random 0.2 0.8))
           (cq/rel-w (dr/random 0.05 0.3))
           (cq/rel-h (dr/random 0.05 0.3))
           (repeatedly (dr/weighted {1 11 2 3 3 1}) gen-mod)))

(defn gen-box-row []
  (let [[w h] [(dr/random 0.4 0.8) (dr/random 0.05 0.15)]
        ul (cq/rel-vec (dr/random 0.1 (- 1 w))
                       (dr/random 0.1 (- 1 h)))
        prototype (rect/rect ul (tm/+ ul (cq/rel-vec w h)))]
    (for [{p :p [w h] :size} (g/subdivide prototype {:rows 1 :cols (dr/random-int 3 12)})]
      (partial box (tm/+ p (tm/* (gv/vec2 w h) 0.5))
               w h
               [(slide (gv/vec2 0 (* h (dr/random 0.5 2.0)))
                       Math/sin (dr/random 0.2 2.0) (dr/random-tau))]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (/ (q/millis) 1000.0)
   :boxes (into (apply concat (repeatedly (dr/random 1 4) gen-box-row))
                (repeatedly (dr/random 7 30) gen-box))})

(defn update-state [state]
  (assoc state :t (/ (q/millis) 1000.0)))

(defn update-box [t box]
  (box t))

(defn draw [{:keys [boxes t]}]
  (q/background 1.0)
  (q/stroke 0.0 0.5)
  (q/fill 0.5 0.2)
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
