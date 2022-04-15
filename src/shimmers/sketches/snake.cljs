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
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:draw-mode :equilateral-links
               :follow-mode :sinusoidal}))

(defn gen-target []
  (let [r (dr/random 0.05 0.15)
        e (* 1 r)]
    (gc/circle (cq/rel-vec (dr/random e (- 1 e)) (dr/random e (- 1 e)))
               (cq/rel-h r))))

(defn gen-segment [segment]
  (let [base (chain/segment-endpoint segment)
        dir-center (tm/- (cq/rel-vec 0.5 0.5) base)
        direction (if (dr/chance 0.66)
                    (+ (g/heading dir-center)
                       (dr/gaussian 0 0.8))
                    (dr/random eq/TAU))
        size (Math/abs (dr/gaussian 20 6))]
    (chain/->KinematicSegment base direction size)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [start (gv/vec2 (g/point-at (cq/screen-rect 0.85) (dr/random)))]
    {:t 0.0
     :target (gen-target)
     :chain (->> (chain/->KinematicSegment start (dr/random eq/TAU) 8)
                 (iterate gen-segment)
                 (take 32)
                 chain/->KinematicChain)}))

(def follow-modes
  {:proportional
   (fn follow-proportional [pos {:keys [p r]} t]
     (let [[x y] (tm/* pos 0.01)
           n (q/noise x y (* 0.01 t))]
       (tm/mix pos (tm/+ p (v/polar (* 1.8 r n) (* 0.5 t)))
               (* 0.04 n))))
   :sinusoidal
   (fn follow-sinusoidal [pos {:keys [p]} t]
     (let [dirv (tm/- p pos)
           speed (* 2 (+ 0.9 (* (Math/sin (* 0.9 t)) (Math/cos (* 0.5 t)))))]
       (tm/+ pos (tm/normalize (g/rotate dirv (Math/sin (* 2 t))) speed))))})

;; TODO: fix how harsh the transition is between old target and new target somehow?
(defn update-state [{:keys [chain target t] :as state}]
  (let [tip (chain/segment-endpoint (last (:segments chain)))
        follow (get follow-modes (:follow-mode @ui-state))]
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
    (q/stroke (tm/smoothstep* 0.3 0.8 (q/noise x y (* 0.01 t)))
              (* 0.3 (tm/smoothstep* 0.1 0.9 (q/noise x y (+ 100 (* 0.01 t)))))))
  (let [edges (g/edges chain)]
    (q/begin-shape :triangles)
    (doseq [[i [a b]] (map-indexed vector edges)
            :let [[x y] (tm/* a 0.001)]]
      (q/fill (tm/smoothstep* 0.25 0.6 (q/noise x y (* t 0.001)))
              (* 0.3 (tm/smoothstep* 0.1 0.9 (q/noise x y (+ 200 (* 0.01 t))))))
      (apply q/vertex a)
      (apply q/vertex b)
      (apply q/vertex (tm/+ b (g/rotate (tm/- a b) (* (if (even? i) 1 -1) (/ eq/TAU 6))))))
    (q/end-shape)))

(defn draw-chain [{:keys [chain]}]
  (q/stroke 0.0 0.05)
  (q/no-fill)
  (cq/draw-path (g/vertices chain)))

(def draw-modes
  {:chain draw-chain
   :brushes draw-triangle-brushes
   :equilateral-links draw-equilateral-links})

(defn ui-controls []
  [:div.readable-width
   [:p "Drag a kinematic chain across a canvas, drawing triangles or lines along it's path."]
   (ctrl/change-mode ui-state (keys draw-modes) {:mode-key :draw-mode})
   [:ul
    [:li "Equilateral links draws triangles from each link in the chain."]
    [:li "Chain draws the lines between each link of the chain."]
    [:li "Brushes draws triangles at each vertex of the chain, spinning them
     proportional to distance from the head of the chain."]]
   (ctrl/change-mode ui-state (keys follow-modes) {:mode-key :follow-mode})
   [:ul
    [:li "Sinusoidal chooses a winding path towards each target locations."]
    [:li "Proportional follows the path mixing from the current location to a
     circle around the target."]]])

(defn draw [state]
  ((get draw-modes (:draw-mode @ui-state)) state))

(sketch/defquil snake
  :created-at "2022-04-02"
  :size [1024 768]
  :on-mount #(ctrl/mount ui-controls)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])1
