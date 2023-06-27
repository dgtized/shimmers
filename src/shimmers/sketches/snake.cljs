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
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:draw-mode :equilateral-links
               :follow-mode :sinusoidal
               :debug false
               :color true
               :limit-palette false
               :spinners false}))

(defn gen-target []
  (let [r (dr/random 0.1 0.3)]
    (gc/circle (cq/rel-vec (dr/random r (- 1 r)) (dr/random r (- 1 r)))
               (cq/rel-h (* 0.33 r)))))

(defn fresh-target
  "Bias next target to be furthest from tip"
  [tip]
  (->> gen-target
       (repeatedly 8)
       (dr/weighted-by (fn [{p :p}] (g/dist-squared p tip)))))

(defn gen-segment [segment]
  (let [base (chain/segment-endpoint segment)
        dir-center (tm/- (cq/rel-vec 0.5 0.5) base)
        direction (if (dr/chance 0.85)
                    (+ (g/heading dir-center)
                       (dr/gaussian 0 0.8))
                    (dr/random-tau))
        size (tm/clamp (abs (dr/gaussian 18 10)) 2.0 36.0)]
    (chain/->KinematicSegment base direction size)))

(defrecord Spinner [pos vel t0 t1])

(defn gen-spinners [chain t]
  (for [[i [a b]] (map-indexed vector (take-last 8 (g/edges chain)))
        :when (dr/chance 0.002)
        :let [dir (if (even? i) 1 -1)
              tip (g/rotate (tm/- a b) (* dir (/ eq/TAU 6)))
              [x y] (tm/- b a)
              f (tm/cross (gv/vec3 x y 0) (gv/vec3 0 0 (* dir 0.18)))]]
    (->Spinner (tm/+ b tip) (gv/vec2 (:x f) (:y f))
               t (+ t (dr/random 0.2 1.4)))))

;; 32 is capacity for active spinners
(defn update-spinners [spinners t]
  (for [{:keys [pos vel t1] :as spin} (take 32 spinners)
        :when (< t t1)]
    (assoc spin
           :pos (tm/+ pos vel)
           :vel (tm/* vel 0.96))))

(defn setup []
  (q/noise-seed (dr/random-int 100000))
  (let [start (gv/vec2 (g/point-at (cq/screen-rect 0.85) (dr/random)))]
    {:image (q/create-graphics (q/width) (q/height))
     :t 0.0
     :target (gen-target)
     :chain (->> (chain/->KinematicSegment start (dr/random-tau) 8)
                 (iterate gen-segment)
                 (take 40)
                 chain/->KinematicChain)
     :spinners []}))

(def follow-modes
  {:proportional
   (fn follow-proportional [pos {:keys [p r]} t]
     (let [[x y] (tm/* pos 0.01)
           n (q/noise x y (* 0.01 t))]
       (tm/mix pos (v/+polar p (* 1.8 r n) (* 0.5 t))
               (* 0.04 n))))
   :sinusoidal
   (fn follow-sinusoidal [pos {:keys [p]} t]
     (let [dirv (tm/- p pos)
           speed (+ (+ 0.5 (* 0.5 (eq/unit-cos (* 0.25 t))))
                    (* (Math/sin (* 0.9 t)) (Math/cos (* 0.5 t))))]
       (tm/+ pos (tm/normalize (g/rotate dirv (* 1.33 (Math/sin (* 2 t))))
                               (* 2.0 speed)))))})

;; TODO: fix how harsh the transition is between old target and new target somehow?
(defn update-state [{:keys [chain target t] :as state}]
  (let [tip (chain/segment-endpoint (last (:segments chain)))
        follow (get follow-modes (:follow-mode @ui-state))]
    (-> (if (g/contains-point? target tip)
          (assoc state :target (fresh-target tip))
          (update state :chain chain/chain-update nil (follow tip target t)))
        (update :spinners update-spinners t)
        (update :spinners into (gen-spinners chain t))
        (update :t + 0.01))))

(defn apply-stroke [target t]
  (let [[x y] (tm/* (:p target) 0.1)]
    (q/stroke (tm/smoothstep* 0.3 0.8 (q/noise x y (* 0.01 t)))
              (* 0.3 (tm/smoothstep* 0.1 0.9 (q/noise x y (+ 100 (* 0.01 t))))))))

;; https://www.youtube.com/watch?v=f4s1h2YETNY led me to:
;; https://iquilezles.org/articles/palettes/
;; outputs an RGB triplet ranged from 0.0 to 1.0
(defn smooth-palette [[ax ay az] [bx by bz] [cx cy cz] [dx dy dz] t]
  (gv/vec3 (+ ax (* bx (Math/cos (* eq/TAU (+ (* cx t) dx)))))
           (+ ay (* by (Math/cos (* eq/TAU (+ (* cy t) dy)))))
           (+ az (* bz (Math/cos (* eq/TAU (+ (* cz t) dz)))))))

(def palette
  (partial smooth-palette
           (gv/vec3 0.5 0.5 0.5)
           (gv/vec3 0.5 0.5 0.5)
           (gv/vec3 1.0 1.0 1.0)
           (gv/vec3 0.0 0.1 0.2)))

(comment (map (fn [t] [t (palette t)]) (range 0.0 2.0 0.05)))

(defn apply-fill [{:keys [color limit-palette]} t vertex]
  (let [[x y] (tm/* vertex 0.001)
        grey (tm/smoothstep* 0.2 0.6 (q/noise x y (* t 0.001)))
        opacity (* 0.3 (tm/smoothstep* 0.1 0.9 (q/noise x y (+ 200 (* 0.01 t)))))]
    (-> (cond (and color limit-palette (< 0.3 grey 0.7))
              (palette (q/noise (+ 60 y) (* 0.001 t) (+ 25 x)))
              (and color (< 0.4 grey 0.6))
              [(mod (* tm/PHI (q/noise x (+ 80 (* 0.02 t)) y)) 1) 0.5 0.5]
              :else
              [grey])
        (conj opacity))))

(defn brush-at [pos theta size]
  (let [t (triangle/inscribed-equilateral {:p (gv/vec2) :r size} theta)]
    (doseq [p (:points t)]
      (apply q/vertex (tm/+ p pos)))))

(defn draw-triangle-brushes [{:keys [target chain t]}]
  (apply-stroke target t)
  (let [vertices (g/vertices chain)
        len (g/dist (first vertices) (last vertices))]
    (q/begin-shape :triangles)
    (doseq [[vertex next-vertex] (partition 2 1 vertices)]
      (apply q/fill (apply-fill @ui-state t vertex))
      (brush-at vertex
                (+ (g/heading (tm/- next-vertex vertex))
                   (* t 0.33 (/ (g/dist vertex (last vertices)) len)))
                (/ (g/dist vertex next-vertex) 1.5)))
    (q/end-shape)))

(defn draw-spinners [{:keys [spinners t]}]
  (q/no-stroke)
  (doseq [{:keys [pos t0 t1]} spinners
          :let [pct (/ (- t t0) (- t1 t0))]]
    (q/fill 0.0 0.12)
    (-> (gt/equilateral2 (- tm/SQRT2) tm/SQRT3)
        (g/scale-size tm/PHI)
        (g/rotate (* 10 pct))
        (g/translate (v/polar (* 16 (tm/smoothstep* 0.1 1.0 pct))
                              (* pct 16)))
        (g/translate pos)
        :points
        cq/draw-triangle)))

(defn equilateral-point [a b i scale]
  (let [a-b (tm/normalize (tm/- a b)
                          (* (/ tm/SQRT3 2)
                             scale
                             (tm/mag (tm/- a b))))]
    (tm/+ (tm/mix a b 0.5)
          (g/rotate a-b (* (if (even? i) 1 -1) (/ eq/TAU 4))))))

(defn test-rotation [t]
  (q/stroke 0 1.0)
  (q/fill 1.0 1.0)
  (let [a (gv/vec2 45 52)
        b (gv/vec2 30 30)]
    (cq/draw-triangle a b (equilateral-point a b 0 t)))
  (let [a (gv/vec2 120 20)
        b (gv/vec2 80 30)]
    (cq/draw-triangle a b (equilateral-point a b 0 t))))

(defn draw-equilateral-links [{:keys [target chain t] :as state}]
  ;; (test-rotation tm/HALF_PI)
  (apply-stroke target t)
  (let [edges (g/edges chain)
        scale-noise (apply q/noise (tm/+ (tm/* (gv/vec3 (ffirst edges) (* 5 t)) 0.001)
                                         (gv/vec3 80 30 60)))
        scale (cond (< 0.4 scale-noise 0.6)
                    1.0
                    (< scale-noise 0.4)
                    (tm/smoothstep* 0.15 0.4 scale-noise)
                    :else
                    (- 1.0 (tm/smoothstep* 0.6 0.85 scale-noise)))]
    (q/begin-shape :triangles)
    (doseq [[i [a b]] (map-indexed vector edges)
            :let []]
      (apply q/fill (apply-fill @ui-state t a))
      (apply q/vertex a)
      (apply q/vertex b)
      (apply q/vertex (equilateral-point a b i scale)))
    (q/end-shape)
    (when (:spinners @ui-state)
      (draw-spinners state))))

(defn draw-chain [{:keys [chain t]}]
  (let [vertices (g/vertices chain)
        [x y] (tm/* (first vertices) 0.005)
        grey (tm/smoothstep* 0.33 0.66 (q/noise x y (* t 0.01)))
        opacity (+ 0.025 (* 0.1 (tm/smoothstep* 0.1 0.9 (q/noise x y (+ 200 (* 0.01 t))))))]
    (apply q/stroke
           (if (and (:color @ui-state) (< 0.4 grey 0.6))
             [(mod (* tm/PHI (q/noise x (+ 80 (* 0.01 t)) y)) 1) 0.5 0.5 opacity]
             [grey opacity]))
    (q/no-fill)
    (cq/draw-curve-path vertices)))

(def draw-modes
  {:chain draw-chain
   :brushes draw-triangle-brushes
   :equilateral-links draw-equilateral-links})

(defn ui-controls []
  [:div
   [:p.readable-width
    "Drag a " [:a {:href "https://en.wikipedia.org/wiki/Kinematic_chain"}
               "kinematic chain"]
    " across a canvas, drawing triangles or lines along it's path."]
   (ctrl/change-mode ui-state (keys draw-modes) {:mode-key :draw-mode})
   [:ul.readable-width
    [:li "Equilateral links draws triangles from each link in the chain."]
    [:li "Chain draws the lines between each link of the chain."]
    [:li "Brushes draws triangles at each vertex of the chain, spinning them
     proportional to distance from the head of the chain."]]
   [:div.flexmodes
    (ctrl/checkbox ui-state "Color Patches" [:color])
    (when (and (#{:equilateral-links :brushes} (:draw-mode @ui-state))
               (:color @ui-state))
      (ctrl/checkbox ui-state "Limit Palette" [:limit-palette]))
    (when (= (:draw-mode @ui-state) :equilateral-links)
      (ctrl/checkbox ui-state "Add Spinners" [:spinners]))]
   [:p]
   (ctrl/change-mode ui-state (keys follow-modes) {:mode-key :follow-mode})
   [:ul.readable-width
    [:li "Sinusoidal chooses a winding path towards each target locations."]
    [:li "Proportional follows the path mixing from the current location to a
     circle around the target."]]
   [ctrl/checkbox ui-state "Debug Mode" [:debug]]])

(defn draw [{:keys [image target] :as state}]
  (q/with-graphics image
    (q/color-mode (if (:limit-palette @ui-state) :rgb :hsl) 1.0)
    ((get draw-modes (:draw-mode @ui-state)) state))
  (q/color-mode (if (:limit-palette @ui-state) :rgb :hsl) 1.0)
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/image image 0 0)
  (when (:debug @ui-state)
    (q/no-fill)
    (q/stroke 0.1 0.5)
    (cq/circle target)))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [1024 768]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ui-controls]])

(sketch/definition snake
  {:created-at "2022-04-02"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))
