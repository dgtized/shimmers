(ns shimmers.sketches.snake
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.math.core :as tm]))

(defn gen-target []
  (let [r (dr/random 0.05 0.1)
        e (* 1 r)]
    (gc/circle (cq/rel-vec (dr/random e (- 1 e)) (dr/random e (- 1 e)))
               (cq/rel-h r))))

(defn gen-segment [{:keys [angle] :as segment}]
  (let [base (chain/segment-endpoint segment)]
    (chain/->KinematicSegment base (dr/gaussian angle 0.5)
                              (Math/abs (dr/gaussian 18 5)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0
   :target (gen-target)
   :chain (->> (chain/->KinematicSegment (:p (gen-target))
                                         (dr/random eq/TAU) 8)
               (iterate gen-segment)
               (take 32)
               chain/->KinematicChain)})

(defn follow [pos {:keys [p r]} t]
  (let [[x y] (tm/* pos 0.01)
        n (q/noise x y (* 0.01 t))]
    (tm/mix pos (tm/+ p (v/polar (* 1.8 r n) t))
            (* 0.03 n))))

(defn update-state [{:keys [chain target t] :as state}]
  (let [tip (chain/segment-endpoint (last (:segments chain)))]
    (-> (if (g/contains-point? target tip)
          (assoc state :target (gen-target))
          state)
        (update :chain chain/chain-update nil (follow tip target t))
        (update :t + 0.01))))

(defn brush-at [pos theta]
  (let [t (gt/triangle2 [-9 -6] [6 10] [-15 5])]
    (doseq [p (:points (g/rotate t theta))]
      (apply q/vertex (tm/+ p pos)))))

(defn draw-triangle-brushes [{:keys [target chain t]}]
  (let [[x y] (tm/* (:p target) 0.1)]
    (q/stroke (tm/smoothstep* 0.3 0.7 (q/noise x y (* 0.01 t)))
              (* 0.2 (q/noise x y (+ 100 (* 0.01 t))))))
  (let [vertices (g/vertices chain)]
    (q/begin-shape :triangles)
    (doseq [vertex vertices
            :let [[x y] (tm/* vertex 0.001)]]
      (q/fill (q/noise x y (* t 0.001))
              0.03)
      (brush-at vertex (* eq/TAU t (/ 50 (g/dist vertex (:p target))))))
    (q/end-shape)))

(defn draw-equilateral-links [{:keys [target chain t]}]
  (let [[x y] (tm/* (:p target) 0.1)]
    (q/stroke (tm/smoothstep* 0.3 0.7 (q/noise x y (* 0.01 t)))
              (* 0.3 (tm/smoothstep* 0.1 0.9 (q/noise x y (+ 100 (* 0.01 t)))))))
  (let [edges (g/edges chain)]
    (q/begin-shape :triangles)
    (doseq [[i [a b]] (map-indexed vector edges)
            :let [[x y] (tm/* a 0.001)]]
      (q/fill (tm/smoothstep* 0.3 0.7 (q/noise x y (* t 0.001)))
              (* 0.3 (tm/smoothstep* 0.1 0.9 (q/noise x y (+ 200 (* 0.01 t))))))
      (apply q/vertex a)
      (apply q/vertex b)
      (apply q/vertex (tm/+ b (g/rotate (tm/- a b) (* (if (even? i) 1 -1) (/ eq/TAU 6))))))
    (q/end-shape)))

(defn draw-chain [{:keys [chain]}]
  (q/stroke 0.0 0.05)
  (q/no-fill)
  (cq/draw-path (g/vertices chain)))

(defonce ui-state (ctrl/state {:mode :equilateral-links}))

(def modes {:chain draw-chain
            :brushes draw-triangle-brushes
            :equilateral-links draw-equilateral-links})

(defn ui-controls []
  (ctrl/change-mode ui-state (keys modes)))

(defn draw [state]
  ((get modes (:mode @ui-state)) state))

(sketch/defquil snake
  :created-at "2022-04-02"
  :size [900 600]
  :on-mount #(ctrl/mount ui-controls)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
