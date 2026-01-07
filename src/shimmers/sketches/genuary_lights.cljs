(ns shimmers.sketches.genuary-lights
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.sketches.genuary :as genuary]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:debug false :lights false}))

(defn gen-particle [bounds]
  (let [new-target (fn [] (rp/sample-point-inside bounds))
        pos (new-target)]
    {:pos pos
     :vel (gv/vec2)
     :lumen (dr/random 0.01 0.33)
     :shape (let [circle (gc/circle (gv/vec2)
                                    (dr/random 3.25 6.5))]
              (case (dr/weighted {:triangle 1.0 :circle 1.0 :square 1.0})
                :triangle (triangle/inscribed-equilateral circle (dr/random-tau))
                :circle circle
                :square (g/scale-size (g/bounds circle) 0.9)))
     :target (new-target)
     :color false
     :new-target new-target}))

(defn update-particle
  [{:keys [pos vel target new-target] :as particle} dt]
  (let [mag (min (tm/mag-squared (tm/- target pos)) 30000)]
    (if (< mag 3.0)
      (if (:lights @ui-state)
        (assoc particle :vel (gv/vec2))
        (assoc particle :target (new-target)))
      (let [force (tm/limit (tm/* (tm/- target pos) (/ 1.0 mag))
                            (* 0.05 (math/sqrt mag)))
            pos' (tm/+ pos (tm/+ (tm/* vel dt) (tm/* force (* 0.5 dt dt))))
            vel' (tm/* (tm/+ vel (tm/* force dt)) 0.95)]
        (-> particle
            (assoc :pos pos')
            (assoc :vel vel')
            (assoc :color (> (tm/mag-squared vel') mag))
            (assoc :last-pos pos))))))

(defn update-particles [particles dt]
  (mapv (fn [x] (update-particle x dt)) particles))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:t (q/millis)
     :light 0.0
     :paths (genuary/word-paths bounds "Genuary")
     :particles (repeatedly 300 (partial gen-particle bounds))}))

(defn new-target [particles pos-f]
  (mapv (fn [particle] (assoc particle :target (pos-f)))
        particles))

(defn change-targets [state lights]
  (update state :particles new-target
          (if lights
            (fn [] (rp/sample-point-inside (:strip (dr/rand-nth (:paths state)))))
            (fn [] (rp/sample-point-inside (cq/screen-rect))))))

(defn run-sim [state steps dt]
  (last (take steps (iterate (fn [s] (update s :particles update-particles dt))
                             state))))

(defn update-state [light-switch]
  (fn [{:keys [t] :as state}]
    (let [target (if (:lights @ui-state) 1.0 0.0)
          dt (- (q/millis) t)]
      (-> (if (:changed @light-switch)
            (do (swap! light-switch assoc :changed false)
                (change-targets state (:lights @ui-state)))
            state)
          (update :light (fn [curr] (+ (* 0.9 curr) (* 0.125 target))))
          (run-sim 16 (/ dt 16.0))
          (assoc :t (q/millis))))))

(defn draw [{:keys [light particles]}]
  (q/ellipse-mode :radius)
  (q/background light)
  (q/no-stroke)
  (doseq [{:keys [pos vel shape color lumen]} particles]
    (if color
      (q/fill 0.0 1.0 0.5 1.0)
      (q/fill (* lumen (tm/clamp (- 1.0 light) 0.2 0.8))))
    (qdg/draw (g/translate (g/rotate shape (g/heading vel))
                           pos))))

(defn page []
  (let [light-switch (atom {:changed false})]
    [:div
     (sketch/component
       :size [800 600]
       :setup setup
       :update (update-state light-switch)
       :draw draw
       :middleware [m/fun-mode framerate/mode])
     [:div.centered.readable-width
      [:p "Genuary 2026 - Day 6 - Lights On/Off"]
      [ctrl/checkbox ui-state "Lights" [:lights]
       {:on-change (fn [_] (swap! light-switch assoc :changed true))}]
      ;; [ctrl/checkbox ui-state "Debug" [:debug]]
      [:p "Little bit of a repeat of assets from the Day 5 sketch."]]]))

(sketch/definition genuary-lights
  {:created-at "2026-01-06"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))
