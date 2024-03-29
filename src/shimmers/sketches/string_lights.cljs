(ns shimmers.sketches.string-lights
  "Concept is bokah circles from out of focus string lights in a backdrop."
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; modify fill/size opacity by y-pos or time to animate?
(defn string-line [p q n r]
  (for [point (repeatedly n #(tm/mix p q (dr/random)))]
    {:point (rp/confusion-disk point r)
     :fill [(dr/gaussian 0.125 0.03) 0.6 0.75 1.0]
     :size (dr/rand-nth [8 9 10 11 12])
     :rate (dr/random 0.2 0.9)}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:strings
   (for [x (range 0.15 0.95 0.1)]
     (string-line (gv/vec2 x 0.05) (gv/vec2 (- x 0.05) 0.95)
                  (* 150 x) (* 0.025 x)))})

(defn update-state [{:keys [strings] :as state}]
  (let [t (/ (q/frame-count) 60)
        strings
        (for [string strings]
          (for [{:keys [point rate fill] :as light} string
                :let [[_ y] point
                      center (eq/unit-cos (* t rate))
                      alpha (eq/gaussian 0.8 center 0.25 y)]]
            (assoc light :fill (assoc fill 3 alpha))))]
    (assoc state :strings strings)))

(defn draw [{:keys [strings]}]
  (q/background 0.15)
  (q/no-stroke)
  (doseq [string strings]
    (doseq [{:keys [point fill size]} string]
      (apply q/fill fill)
      (cq/circle (cq/rel-vec point) size))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition string-lights
    {:created-at "2021-08-31"
     :tags #{:deterministic}
     :type :quil}
  (ctrl/mount page))
