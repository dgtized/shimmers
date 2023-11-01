(ns shimmers.sketches.woven
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:screen-size "800x600"}))

(defn add-limit [{:keys [pos limit] :as inst}]
  (if (dr/chance 0.4)
    (assoc inst :limit (tm/mix pos limit (dr/random 0.55 0.95)))
    inst))

(defn gen-threads [n pass]
  (for [t (range n)]
    (let [o (+ (/ (float (inc t)) (inc n)) (dr/gaussian 0.0 (/ 0.33 (inc n))))]
      (-> (case (mod pass 4)
            0 {:pos (cq/rel-vec -0.1 o) :dir v/right :limit (cq/rel-vec 1.1 o)}
            1 {:pos (cq/rel-vec o -0.1) :dir v/up :limit (cq/rel-vec o 1.1)}
            2 {:pos (cq/rel-vec 1.1 o) :dir v/left :limit (cq/rel-vec -0.1 o)}
            3 {:pos (cq/rel-vec o 1.1) :dir v/down :limit (cq/rel-vec o -0.1)})
          (assoc :rot 0.0)
          add-limit))))

(defn gen-n []
  (dr/weighted {5 2
                7 5
                9 1
                11 2
                13 1
                17 1}))

(defn choose-rate []
  (dr/random 0.0002 0.01))

(def black&white
  [0.0 0.0 0.0])

(defn random-color [mono]
  (dr/weighted
   {[0.6 0.8 0.5] (if mono 0 1)
    [0.0 0.8 0.5] (if mono 0 1)
    black&white 4}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/noise-seed (dr/seed))
  (let [pass 0
        n (gen-n)
        mono (dr/chance 0.5)]
    {:seed (cq/rel-vec (dr/random-vertex))
     :n n
     :pass pass
     :rate (choose-rate)
     :triangles (gen-threads n pass)
     :screen (cq/screen-rect)
     :color (random-color mono)
     :mono mono
     :t (q/millis)}))

(defn passed? [{:keys [pos dir limit]}]
  (case dir
    v/right (> (:x pos) (:x limit))
    v/left (< (:x pos) (:x limit))
    v/up (> (:y pos) (:y limit))
    v/down (< (:y pos) (:y limit))))

(defn outside? [screen {:keys [pos] :as instance}]
  (and (not (g/contains-point? screen pos))
       (passed? instance)))

(defn update-pos [rate t dt {:keys [dir] :as inst}]
  (-> inst
      (update :pos tm/+ (tm/* dir (* 0.075 dt)))
      (update :rot + (* (* 0.005 (Math/sin (* rate t))) dt))))

(defn update-state [{:keys [mono triangles rate screen pass t] :as state}]
  (let [dt (- (q/millis) t)]
    (-> (if (every? (partial outside? screen) triangles)
          (let [n (gen-n)]
            (-> state
                (update :pass inc)
                (assoc :n n
                       :rate (choose-rate)
                       :color (random-color mono)
                       :triangles (gen-threads n (inc pass)))))
          state)
        (update :t + dt)
        (update :seed tm/+ (tm/* (gv/vec2 0.00001 0.00001) t))
        (update :triangles (partial map (partial update-pos rate t dt))))))

(defn draw [{:keys [seed pass color triangles n]}]
  (if (< pass 4)
    (let [r (cq/rel-h (/ 0.15 (inc n)))]
      (doseq [{:keys [pos rot] :as inst} triangles]
        (when-not (passed? inst)
          (let [n (apply q/noise (tm/* (tm/+ seed pos) 0.006))
                n2 (apply q/noise (tm/+ (gv/vec2 50 50) (tm/* (tm/+ seed pos) 0.005)))]
            (q/fill 0.0 (+ 0.0001 (* n 0.0225)))
            (apply q/stroke (conj color (+ 0.001 (* n 0.225))))
            (let [triangle (triangle/inscribed-equilateral pos (* (+ 0.25 (* 2.25 n2)) r) rot)]
              (cq/draw-triangle (g/vertices triangle)))))))
    (q/no-loop)))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div
    [:p.readable-width
     "Create parallel spinning triangle trails from each of the four cardinal
     directions. Prefer monochromatic color schemes but occasionally introduce
     blue or red stroke coloring. Each trail's opacity is modulated by noise at
     that position, as is the size of each triangle. The spin of each parallel
     set of trails is determined before each pass. The piece juxtaposes the
     ordered repitition of the parallel trails with the chaos of the noise,
     giving some local variations in texture while maintaining an overall sense
     of uniformity."]
    [ctrl/container
     [ctrl/dropdown ui-state "Screen Size" [:screen-size]
      (screen/sizes)
      {:on-change #(view-sketch/restart-sketch :woven)}]]]])

(sketch/definition woven
  {:created-at "2023-10-25"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
