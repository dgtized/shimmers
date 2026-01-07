(ns shimmers.sketches.genuary-lights
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketches.genuary :as genuary]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.core :as g]
   [shimmers.algorithm.random-points :as rp]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.common.quil-draws-geom :as qdg]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:debug false :lights false}))
(defonce light-switch (atom {:changed false}))

(defn gen-particle [bounds]
  (let [pos (rp/sample-point-inside bounds)]
    {:pos pos
     :last-pos pos
     :shape (let [circle (gc/circle (gv/vec2)
                                    (dr/random 3.0 6.0))]
              (case (dr/weighted {:triangle 1.0 :circle 1.0 :square 1.0})
                :triangle (triangle/inscribed-equilateral circle (dr/random-tau))
                :circle circle
                :square (g/scale-size (g/bounds circle) 0.9)))
     :target (rp/sample-point-inside bounds)}))

(defn update-particle [{:keys [pos last-pos target] :as particle} dt]
  (if (tm/delta= pos target)
    particle
    (let [acc (tm/normalize (tm/- target pos))
          velocity (tm/limit (tm/- pos last-pos) (/ (g/dist pos target) 10.0))]
      (-> particle
          (update :pos tm/+ (tm/+ velocity (tm/* acc 0.1 dt)))
          (assoc :last-pos pos)))))

(defn update-particles [particles dt]
  (mapv (fn [x] (update-particle x dt)) particles))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:t 0.0
     :light 0.0
     :paths (genuary/word-paths bounds "Genuary")
     :particles (repeatedly 200 (partial gen-particle bounds))}))

(defn new-target [particles pos-f]
  (mapv (fn [particle] (assoc particle :target (pos-f)))
        particles))

(defn change-targets [state lights]
  (update state :particles new-target
          (if lights
            (fn [] (rp/sample-point-inside (:strip (dr/rand-nth (:paths state)))))
            (fn [] (rp/sample-point-inside (cq/screen-rect))))))

(defn update-state [state]
  (let [target (if (:lights @ui-state) 1.0 0.0)]
    (-> (if (:changed @light-switch)
          (do (swap! light-switch assoc :changed false)
              (change-targets state (:lights @ui-state)))
          state)
        (update :light (fn [curr] (+ (* 0.85 curr) (* 0.15 target))))
        (update :particles update-particles 0.001)
        (update :t + 0.01))))

(defn draw [{:keys [light particles]}]
  (q/background light)
  (q/color (- 1.0 light))
  (q/fill (* 0.1 (- 1.0 light)))
  (doseq [{:keys [pos last-pos shape]} particles]
    (qdg/draw (g/translate (g/rotate shape (g/heading (tm/- pos last-pos)))
                           pos))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.centered.readable-width
    [:p "Genuary 2026 - Day 6 - Lights On/Off"]
    [ctrl/checkbox ui-state "Lights" [:lights]
     {:on-change (fn [_] (swap! light-switch assoc :changed true))}]
    [ctrl/checkbox ui-state "Debug" [:debug]]
    [:p "Little bit of a repeat of assets from the Day 5 sketch."]]])

(sketch/definition genuary-lights
  {:created-at "2026-01-06"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))
