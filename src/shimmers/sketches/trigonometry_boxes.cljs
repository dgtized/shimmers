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
   [shimmers.math.geometry :as geometry]
   [shimmers.math.vector :as v]
   [shimmers.math.wave :as wave]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn box [center width height modulations fill t]
  (let [{:keys [center width height angle]}
        ((apply comp modulations) {:center center :width width :height height :angle 0} t)
        wh (gv/vec2 (* width 0.5) (* height 0.5))
        s (-> (rect/rect (tm/- center wh) (tm/+ center wh))
              (geometry/rotate-around-centroid angle))]
    (if fill
      (assoc s :fill fill)
      s)))

(defn slide [v f dt t0]
  (fn [box t]
    (update box :center tm/+ (tm/* v (f (+ t0 (* t dt)))))))

(defn jitter [amount threshold f dt t0]
  (fn [box t]
    (update box :center tm/+
            (dr/jitter (* amount (tm/smoothstep* threshold 1.0 (f (+ t0 (* t dt)))))))))

(defn jitter-rotate [amount threshold f dt t0]
  (fn [box t]
    (update box :angle (if (dr/chance 0.5) + -)
            (* (dr/random amount) (tm/smoothstep* threshold 1.0 (f (+ t0 (* t dt))))))))

(defn rotate [amount f dt t0]
  (fn [box t]
    (assoc box :angle (* amount (f (+ t0 (* t dt)))))))

(defn orbit [r dt t0]
  (fn [box t]
    (update box :center tm/+ (v/polar r (+ t0 (* dt t))))))

(defn resize [[dx dy] f dt t0]
  (fn [box t]
    (-> box
        (update :width + (* dx (f (+ t0 (* t dt)))))
        (update :height + (* dy (f (+ t0 (* t dt))))))))

(defn gen-time-function []
  (dr/weighted [[Math/sin 4.0] [Math/cos 4.0] [Math/tan 1.0]
                [(partial wave/triangle eq/TAU) 2.0]
                [(partial wave/square (dr/random 0.75 3.5)) 1.0]
                [(partial wave/sawtooth eq/TAU) 2.0]
                [(fn [t] (- 0.0 (wave/sawtooth eq/TAU t))) 2.0]]))

(defn gen-mod []
  (let [modf (dr/weighted {:slide 2.0 :resize 1.0 :jitter 2.0
                           :rotate 1.0 :jitter-rotate 1.0
                           :orbit 1.0})
        tf (gen-time-function)
        dt (dr/random 0.5 1.5)
        t0 (dr/random-tau)]
    (case modf
      :jitter
      (jitter (dr/random 10.0) (dr/random 0.0 0.8) tf dt t0)
      :jitter-rotate
      (jitter-rotate (dr/random 1.0) (dr/random 0.0 0.8) tf dt t0)
      :rotate
      (rotate (dr/random eq/TAU) tf dt t0)
      :orbit
      (orbit (cq/rel-h (dr/random 0.02 0.1)) (* (if (dr/chance 0.5) -1 1) dt) t0)
      (({:slide slide :resize resize} modf)
       (apply cq/rel-vec
              (dr/weighted {[(dr/random 0.05 0.25) 0] 3.0
                            [0 (dr/random 0.05 0.25)] 3.0
                            [(dr/random 0.05 0.25)
                             (dr/random 0.05 0.25)] 1.0}))
       tf dt t0))))

(defn gen-fill []
  (dr/weighted
   [[[(dr/gaussian 0.0 0.05)
      (dr/gaussian 0.6 0.05)
      (dr/gaussian 0.5 0.05)
      0.2] 2.0]
    [[(+ (dr/gaussian 0.0 0.05) (dr/gaussian 0.5 0.05))
      (dr/gaussian 0.6 0.05)
      (dr/gaussian 0.5 0.05)
      0.2] 1.0]
    [[(dr/gaussian 0.5 0.08) 0.2] 1.0]
    [[0.0 0.2] 2.0]
    [[0.1 0.2] 1.0]
    [nil 0.5]]))

(defn gen-box []
  (partial box
           (cq/rel-vec (dr/random 0.2 0.8)
                       (dr/random 0.2 0.8))
           (cq/rel-w (dr/random 0.05 0.3))
           (cq/rel-h (dr/random 0.05 0.3))
           (repeatedly (dr/weighted {1 5 2 4 3 2 4 1 5 1}) gen-mod)
           (gen-fill)))

(defn gen-box-set []
  (let [row? (dr/chance 0.5)
        [w h] (if row?
                [(dr/random 0.4 0.8) (dr/random 0.05 0.25)]
                [(dr/random 0.05 0.25) (dr/random 0.4 0.8)])
        ul (cq/rel-vec (dr/random 0.1 (- 1 w))
                       (dr/random 0.1 (- 1 h)))
        tf (gen-time-function)
        prototype (rect/rect ul (tm/+ ul (cq/rel-vec w h)))
        fill (gen-fill)
        splits (dr/random-int 3 12)
        divisions (if row?
                    {:rows 1 :cols splits}
                    {:rows splits :cols 1})]
    (for [{p :p [w h] :size} (g/subdivide prototype divisions)]
      (partial box (tm/+ p (tm/* (gv/vec2 w h) 0.5))
               w h
               [(slide (if row?
                         (gv/vec2 0 (* h (dr/random 0.5 2.0)))
                         (gv/vec2 (* w (dr/random 0.5 2.0)) 0))
                       tf (dr/random 0.3 3.0) (dr/random-tau))]
               fill))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (/ (q/millis) 1000.0)
   :boxes (into (apply concat (repeatedly (dr/random 1 4) gen-box-set))
                (repeatedly (dr/random 7 22) gen-box))})

(defn update-state [state]
  (assoc state :t (/ (q/millis) 1000.0)))

(defn update-box [t box]
  (box t))

(defn draw [{:keys [boxes t]}]
  (q/background 1.0)
  (q/stroke 0.0 0.5)
  (doseq [box boxes]
    (let [s (update-box t box)]
      (q/fill (:fill s))
      (qdg/draw s))))

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
